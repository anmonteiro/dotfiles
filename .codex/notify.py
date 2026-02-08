#!/usr/bin/env python3

import json
import os
import shlex
import shutil
import subprocess
import sys
from typing import Optional


def should_use_terminal_notifier() -> bool:
    if os.environ.get("CODEX_NOTIFY_FORCE_TERMINAL_NOTIFIER") == "1":
        return True
    if os.environ.get("TERM_PROGRAM") == "ghostty":
        return False
    if os.environ.get("__CFBundleIdentifier") == "com.mitchellh.ghostty":
        return False
    if os.environ.get("TERM") == "xterm-ghostty":
        return False
    return True


def tmux_click_command() -> Optional[str]:
    """Build a command to jump back to the originating tmux window/pane."""
    tmux = shutil.which("tmux")
    tmux_pane = os.environ.get("TMUX_PANE")
    if not tmux or not tmux_pane:
        return None

    try:
        # Target the pane where Codex is running so we capture the right window.
        result = subprocess.run(
            [tmux, "display-message", "-p", "-t", tmux_pane, "#{socket_path}\t#{window_id}\t#{pane_id}"],
            capture_output=True,
            text=True,
            timeout=2,
            check=True,
        )
    except Exception:
        return None

    parts = result.stdout.strip().split("\t")
    if len(parts) != 3 or not all(parts):
        return None

    socket_path, window_id, pane_id = parts
    q_tmux = shlex.quote(tmux)
    q_socket = shlex.quote(socket_path)
    q_window = shlex.quote(window_id)
    q_pane = shlex.quote(pane_id)
    return (
        f"{q_tmux} -S {q_socket} select-window -t {q_window} >/dev/null 2>&1; "
        f"{q_tmux} -S {q_socket} select-pane -t {q_pane} >/dev/null 2>&1"
    )


def notify(title: str, message: str) -> None:
    """Send a notification using terminal-notifier only.

    Minimal, non-blocking and predictable: runs
      terminal-notifier -title <title> -message <message> -group codex
    and ignores failures.
    """
    if not should_use_terminal_notifier():
        return
    tn = shutil.which("terminal-notifier")
    if not tn:
        return
    args = [
        tn,
        "-title",
        title,
        "-message",
        message,
        "-group",
        "codex",
        "-activate",
        "net.kovidgoyal.kitty",
    ]
    click_cmd = tmux_click_command()
    if click_cmd:
        args.extend(["-execute", click_cmd])
    try:
        subprocess.run(args, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=3)
    except Exception:
        pass


def main() -> int:

    raw: Optional[str] = None
    if len(sys.argv) == 2:
        raw = sys.argv[1]
    else:
        # Fallback: try reading JSON from stdin if piped
        if not sys.stdin.isatty():
            raw = sys.stdin.read().strip() or None

    if not raw:
        print("Usage: notify.py <NOTIFICATION_JSON>")
        return 1

    try:
        notification = json.loads(raw)
    except json.JSONDecodeError:
        # Don’t crash the agent pipeline on local format issues
        return 1

    if notification.get("type") != "agent-turn-complete":
        return 0

    assistant_message: Optional[str] = notification.get("last-assistant-message")
    title = f"Codex: {assistant_message}" if assistant_message else "Codex: Turn Complete!"
    input_messages = notification.get("input_messages", [])
    message = " ".join(input_messages).strip()

    notify(title, message)
    return 0


if __name__ == "__main__":
    sys.exit(main())
