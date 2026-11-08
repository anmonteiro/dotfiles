#!/usr/bin/env python3

import argparse
import os
import re
import shlex
import subprocess
import sys


MESSAGES = {
    "done": "Codex finished.",
    "blocked": "Codex needs your input.",
    "error": "Codex encountered an error.",
}


def should_use_terminal_notifier() -> bool:
    if os.environ.get("CODEX_NOTIFY_FORCE_TERMINAL_NOTIFIER") == "1":
        return True
    return not (
        os.environ.get("TERM_PROGRAM") == "ghostty"
        or os.environ.get("__CFBundleIdentifier") == "com.mitchellh.ghostty"
        or os.environ.get("TERM") == "xterm-ghostty"
    )


def main() -> int:
    # A running app-server may still invoke the retired Stop hook.
    if len(sys.argv) == 1:
        return 0

    parser = argparse.ArgumentParser()
    parser.add_argument("--tmux", required=True)
    parser.add_argument("--notifier", required=True)
    parser.add_argument("socket")
    parser.add_argument("pane")
    parser.add_argument("server_pid", type=int)
    parser.add_argument("state")
    parser.add_argument("message", nargs="?", default="")
    args = parser.parse_args()

    if args.state not in MESSAGES or not should_use_terminal_notifier():
        return 0
    if not re.fullmatch(r"%[0-9]+", args.pane):
        return 1

    select = " ; ".join(
        f"{command} -t {args.pane}"
        for command in ("select-window", "select-pane", "switch-client")
    )
    click = shlex.join([
        args.tmux, "-S", args.socket, "if-shell", "-F",
        f"#{{==:#{{pid}},{args.server_pid}}}", select,
    ])
    message = " ".join(args.message.split()) or MESSAGES[args.state]
    if len(message) > 120:
        message = message[:117] + "..."
    try:
        subprocess.run(
            [
                args.notifier,
                "-title", "Codex",
                "-message", message,
                "-group", f"codex:{args.socket}:{args.server_pid}:{args.pane}",
                "-activate", "net.kovidgoyal.kitty",
                "-execute", click,
            ],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            timeout=3,
        )
    except (OSError, subprocess.TimeoutExpired):
        pass
    return 0


if __name__ == "__main__":
    sys.exit(main())
