import argparse
import base64
import json
import os
from pathlib import Path
import shlex
import subprocess
import sys
import tempfile
import time


def wait_for(predicate):
    deadline = time.monotonic() + 5
    while time.monotonic() < deadline:
        if predicate():
            return
        time.sleep(0.02)
    raise AssertionError("condition timed out")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--tmux", required=True)
    parser.add_argument("--notify", required=True)
    parser.add_argument("--config", required=True)
    args = parser.parse_args()
    env = dict(os.environ, CODEX_NOTIFY_FORCE_TERMINAL_NOTIFIER="1")
    env.pop("TMUX", None)
    env.pop("TMUX_PANE", None)

    with tempfile.TemporaryDirectory(prefix="tmux-notify-") as directory:
        root = Path(directory)
        socket = str(root / "socket with 'quotes'")
        log = root / "notifications.jsonl"
        notifier = root / "terminal-notifier"
        notifier.write_text(
            f"#!{sys.executable}\n"
            "import json, os, sys\n"
            f"fd = os.open({str(log)!r}, os.O_WRONLY | os.O_CREAT | os.O_APPEND, 0o600)\n"
            "os.write(fd, (json.dumps(sys.argv[1:]) + '\\n').encode())\n"
            "os.close(fd)\n"
        )
        notifier.chmod(0o755)
        runner = root / "notify"
        runner.write_text(
            "#!/bin/sh\nexec "
            + shlex.join([
                sys.executable, args.notify,
                "--tmux", args.tmux, "--notifier", str(notifier),
            ])
            + ' -- "$@"\n'
        )
        runner.chmod(0o755)
        config = root / "tmux.conf"
        config.write_text(
            Path(args.config).read_text().replace("@tmux_notify@", str(runner))
        )
        emitter = root / "emitter.py"
        emitter.write_text(
            "import json, sys\n"
            "for line in sys.stdin:\n"
            "    seq, reports = json.loads(line)\n"
            "    for report in reports:\n"
            "        sys.stdout.write('\\x1b]7501;' + report + '\\x1b\\\\')\n"
            "    sys.stdout.write('\\x1b]2;processed-' + str(seq) + '\\x07')\n"
            "    sys.stdout.flush()\n"
        )
        command = shlex.join([sys.executable, str(emitter)])
        sequence = 0
        client = None

        def tmux(*arguments):
            return subprocess.run(
                [args.tmux, "-S", socket, "-f", str(config), *arguments],
                env=env, check=True, capture_output=True, text=True, timeout=5,
            ).stdout.strip()

        def new_session(name):
            return tmux(
                "new-session", "-d", "-s", name, "-x", "40", "-y", "20",
                "-c", str(root), "-P", "-F", "#{pane_id}", command,
            )

        def emit(pane, *reports):
            nonlocal sequence
            sequence += 1
            tmux("send-keys", "-t", pane, "-l", json.dumps([sequence, reports]))
            tmux("send-keys", "-t", pane, "Enter")
            wait_for(lambda: tmux("display-message", "-p", "-t", pane,
                                 "#{pane_title}") == f"processed-{sequence}")

        def records():
            if not log.exists():
                return []
            return [dict(zip(row[::2], row[1::2]))
                    for row in map(json.loads, log.read_text().splitlines())]

        def expect_count(count):
            wait_for(lambda: len(records()) >= count)
            assert len(records()) == count, records()

        def click(record):
            return subprocess.run(
                ["/bin/sh", "-c", record["-execute"]], env=env,
                capture_output=True, text=True, timeout=5,
            )

        def active(pane):
            return tmux("display-message", "-p", "-t", pane, "#{pane_active}")

        try:
            first = new_session("main")
            assert tmux("show-options", "-gv", "prefix") == "C-a"
            emit(first, "state=done:app=codex")
            expect_count(1)
            first_notification = records()[0]
            assert first_notification["-message"] == "Codex finished."
            print("PASS: a pane's first status report sends a notification", flush=True)

            second = tmux("split-window", "-t", first, "-c", str(root),
                          "-P", "-F", "#{pane_id}", command)
            emit(second, "state=done:app=codex")
            expect_count(2)
            assert records()[0]["-group"] != records()[1]["-group"]
            tmux("resize-pane", "-Z", "-t", second)
            with (root / "client.log").open("w") as client_log:
                client = subprocess.Popen(
                    [args.tmux, "-S", socket, "-C", "attach-session", "-t", "main"],
                    env=env, stdin=subprocess.PIPE, stdout=client_log,
                    stderr=client_log,
                )
                wait_for(lambda: tmux("list-clients", "-F", "#{client_pid}")
                         == str(client.pid))
                assert click(first_notification).returncode == 0
                assert active(first) == "1"
                assert tmux("display-message", "-p", "-t", first,
                            "#{window_zoomed_flag}") == "0"
                print("PASS: click selects the exact pane and exits zoom", flush=True)

                target = new_session("other")
                tmux("join-pane", "-d", "-s", first, "-t", target)
                assert click(first_notification).returncode == 0
                assert active(first) == "1"
                assert tmux("list-clients", "-F", "#{session_name}") == "other"
                print("PASS: click follows a pane moved to another session", flush=True)

                emit(first, "state=done:app=codex", "state=done:app=codex")
                # Both completions happen within one PTY write, between monitor ticks.
                emit(first, "state=working:app=codex", "state=done:app=codex",
                     "state=working:app=codex", "state=done:app=codex")
                expect_count(4)
                print("PASS: rapid turns notify without duplicate status reports", flush=True)

                hostile = f"--help: ' \" $(touch {root}/injected) `true` #{{pane_id}}"
                message = base64.b64encode(hostile.encode()).decode()
                emit(first, f"state=blocked:app=codex:msg={message}",
                     "state=error:app=codex", "state=idle:app=codex")
                expect_count(6)
                messages = {record["-message"] for record in records()[4:]}
                expected_message = " ".join(hostile.split())
                if len(expected_message) > 120:
                    expected_message = expected_message[:117] + "..."
                assert messages == {expected_message, "Codex encountered an error."}, messages
                assert not (root / "injected").exists()
                print("PASS: event snapshots preserve status and safely quote messages", flush=True)

                emit(first, "state=done:app=other", "state=working:app=codex",
                     "state=invalid:app=codex", "state=clear")
                tmux("source-file", str(config))
                emit(first, "state=blocked:app=codex")
                expect_count(7)
                assert records()[-1]["-message"] == "Codex needs your input."
                print("PASS: filtering and configuration reload avoid extra notifications", flush=True)

                tmux("kill-pane", "-t", first)
                assert click(first_notification).returncode != 0
                assert active(target) == "1"
                print("PASS: a closed pane cannot redirect a stale notification", flush=True)

                client.stdin.write(b"detach-client\n")
                client.stdin.flush()
                client.wait(timeout=5)

            tmux("kill-server")
            new_first = new_session("restarted")
            assert new_first == first
            new_second = tmux("split-window", "-t", new_first,
                              "-P", "-F", "#{pane_id}", command)
            click(first_notification)
            assert active(new_second) == "1"
            print("PASS: a restarted server cannot reuse a stale notification target", flush=True)
        finally:
            subprocess.run([args.tmux, "-S", socket, "kill-server"], env=env,
                           stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                           timeout=5)
            if client is not None and client.poll() is None:
                client.terminate()
                client.wait(timeout=5)


if __name__ == "__main__":
    main()
