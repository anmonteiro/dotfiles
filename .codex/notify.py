#!/usr/bin/env python3

import json
import os
import shlex
import shutil
import subprocess
import sys
import time
from typing import Any, Optional


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


def tmux_click_command(notification: dict[str, Any]) -> Optional[str]:
    """Build a command to jump back to the originating tmux window/pane."""
    tmux = shutil.which("tmux")
    if not tmux:
        return None

    deadline = time.monotonic() + 2
    try:
        result = subprocess.run(
            [
                tmux, "list-panes", "-a", "-F",
                "#{socket_path}\t#{window_id}\t#{pane_id}\t#{pane_current_command}\t#{pane_current_path}",
            ],
            capture_output=True,
            text=True,
            timeout=2,
            check=True,
        )
    except (OSError, subprocess.SubprocessError):
        return None

    panes = [line.split("\t", 4) for line in result.stdout.splitlines()]
    panes = [pane for pane in panes if len(pane) == 5 and "codex" in pane[3]]
    session_id = notification.get("session_id") or notification.get("thread-id")
    cwd = notification.get("cwd")
    cwd_panes = [
        pane for pane in panes
        if cwd and os.path.realpath(pane[4]) == os.path.realpath(cwd)
    ]

    # A shared app-server inherits TMUX_PANE from its own launch, not this TUI.
    # Match the session-id in our configured status line, without reading scrollback.
    matches = []
    if session_id:
        for pane in panes:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                return None
            try:
                screen = subprocess.run(
                    [tmux, "-S", pane[0], "capture-pane", "-p", "-t", pane[2], "-S", "0"],
                    capture_output=True,
                    text=True,
                    timeout=remaining,
                    check=True,
                )
            except (OSError, subprocess.SubprocessError):
                continue
            lines = screen.stdout.rstrip().splitlines()
            if lines and session_id in lines[-1].split(" · "):
                matches.append(pane)

    if not matches:
        # A narrow pane can truncate the UUID. Only fall back to the cwd when
        # every matching Codex pane belongs to the same window.
        matches = cwd_panes
    windows = {(pane[0], pane[1]) for pane in matches}
    if len(windows) != 1:
        return None

    socket_path, window_id = windows.pop()
    q_tmux = shlex.quote(tmux)
    q_socket = shlex.quote(socket_path)
    q_window = shlex.quote(window_id)
    command = f"{q_tmux} -S {q_socket} select-window -t {q_window} >/dev/null 2>&1"
    if len(matches) == 1:
        q_pane = shlex.quote(matches[0][2])
        command += f" && {q_tmux} -S {q_socket} select-pane -t {q_pane} >/dev/null 2>&1"
    return command


def notify(title: str, message: str, notification: dict[str, Any]) -> None:
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
    click_cmd = tmux_click_command(notification)
    if click_cmd:
        args.extend(["-execute", click_cmd])
    try:
        subprocess.run(args, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=3)
    except Exception:
        pass


def compact(text: Optional[str], fallback: str, limit: int = 120) -> str:
    value = " ".join((text or "").split()) or fallback
    if len(value) <= limit:
        return value
    return value[: limit - 3] + "..."


def old_notification_message(notification: dict[str, Any]) -> Optional[tuple[str, str]]:
    assistant_message: Optional[str] = notification.get("last-assistant-message")
    if not assistant_message:
        return None
    title = f"Codex: {compact(assistant_message, '')}"
    input_messages = notification.get("input_messages", [])
    message = compact(" ".join(input_messages), "")
    return title, message


def stop_hook_message(notification: dict[str, Any]) -> Optional[tuple[str, str]]:
    assistant_message: Optional[str] = notification.get("last_assistant_message")
    if not assistant_message:
        return None
    title = "Codex"
    message = compact(assistant_message, "")
    return title, message


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

    if notification.get("hook_event_name") == "Stop":
        notification_text = stop_hook_message(notification)
    elif notification.get("type") == "agent-turn-complete":
        notification_text = old_notification_message(notification)
    else:
        return 0

    if not notification_text:
        return 0

    title, message = notification_text
    notify(title, message, notification)
    return 0


if __name__ == "__main__":
    sys.exit(main())
