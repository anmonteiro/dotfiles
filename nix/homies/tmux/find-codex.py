#!/usr/bin/env python3
"""Find a running Codex session in the current tmux server."""

import argparse
from contextlib import closing
import json
import os
from pathlib import Path
import re
import sqlite3
import subprocess
import sys


UUID = re.compile(r"[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}")
FOOTER_ID = re.compile(r" · ([0-9a-fA-F-]{8,36})(…)?(?=\s|$)")


def normalize_id(value):
    value = value.strip().lower()
    if not UUID.fullmatch(value):
        raise ValueError("Enter a full Codex session UUID.")
    return value


def footer_id(text):
    # Ignore UUIDs in the transcript. Warnings and goal progress may follow
    # the ID on the same line, unlike the footer layout in wider Rex panes.
    lines = [line for line in text.splitlines() if line.strip()]
    for line in reversed(lines[-3:]):
        if not re.match(r"^  \S", line) or " · Context " not in line:
            continue
        for match in FOOTER_ID.finditer(line):
            value, shortened = match.groups()
            value = value.lower()
            if shortened and len(value) < 36:
                return value, True
            if UUID.fullmatch(value):
                return value, False
    return None


def unique_local_id(prefix, codex_home):
    try:
        uri = (codex_home / "state_5.sqlite").as_uri() + "?mode=ro"
        with closing(sqlite3.connect(uri, uri=True, timeout=0.2)) as db:
            rows = db.execute(
                "SELECT id FROM threads WHERE id LIKE ? LIMIT 2", (prefix + "%",)
            ).fetchall()
        if len(rows) == 1:
            return normalize_id(rows[0][0])
    except (sqlite3.Error, ValueError):
        pass
    return None


class Finder:
    def __init__(self, tmux, socket=None, codex_home=None):
        self.command = [tmux] + (["-S", socket] if socket else [])
        self.codex_home = Path(
            codex_home or os.environ.get("CODEX_HOME", Path.home() / ".codex")
        ).expanduser().resolve()

    def run(self, *args):
        try:
            return subprocess.run(
                [*self.command, *args], check=True, capture_output=True,
                text=True, timeout=5,
            ).stdout.rstrip("\n")
        except subprocess.CalledProcessError as error:
            raise RuntimeError(error.stderr.strip() or "tmux command failed") from error

    def matches(self, pane, session_id):
        try:
            state = self.run(
                "display-message", "-p", "-t", pane,
                "#{pane_current_command}\t#{pane_dead}",
            )
            if state != "codex\t0":
                return False
            visible = footer_id(self.run("capture-pane", "-p", "-J", "-t", pane))
        except RuntimeError:
            return False  # A pane may close while the lookup is running.
        if visible is None:
            return False
        value, shortened = visible
        if not shortened:
            return value == session_id
        return session_id.startswith(value) and unique_local_id(value, self.codex_home) == session_id

    def find(self, session_id):
        session_id = normalize_id(session_id)
        panes = self.run("list-panes", "-a", "-F", "#{pane_id}").splitlines()
        # A linked window can appear more than once in list-panes -a.
        return [pane for pane in dict.fromkeys(panes) if self.matches(pane, session_id)]

    def label(self, pane):
        text = self.run(
            "display-message", "-p", "-t", pane,
            "#{session_name}:#{window_index}.#{pane_index} · #{pane_current_path}",
        )
        return "".join(char if char.isprintable() else " " for char in text)

    def focus(self, pane, session_id, client=None, close_popup=False):
        if not self.matches(pane, session_id):
            raise ValueError("That pane is no longer showing this session. Try again.")
        client = client or self.run("display-message", "-p", "#{client_name}")
        if not client or client not in self.run("list-clients", "-F", "#{client_name}").splitlines():
            raise ValueError("The tmux client is no longer attached.")
        # A modal popup prevents selecting another pane in its own window.
        # Close it and focus the result in one server-side command sequence,
        # since closing the popup also terminates the helper running inside it.
        commands = ["display-popup", "-C", "-c", client, ";"] if close_popup else []
        # Resolve the pane's current location, even if it moved during the prompt.
        # select-pane also reveals the pane if another pane in its window is zoomed.
        self.run(
            *commands,
            "select-window", "-t", pane, ";",
            "select-pane", "-t", pane, ";",
            "switch-client", "-c", client, "-t", pane,
        )


def choose(finder, panes):
    if len(panes) == 1:
        return panes[0]
    print("This session is open in several panes:")
    for index, pane in enumerate(panes, 1):
        print(f"  {index}. {finder.label(pane)}")
    while True:
        value = input("Pane number (empty to cancel): ").strip()
        if not value:
            return None
        if value.isdecimal() and 1 <= int(value) <= len(panes):
            return panes[int(value) - 1]
        print(f"Choose a number from 1 to {len(panes)}.")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--tmux", default="tmux")
    parser.add_argument("--socket")
    parser.add_argument("--client")
    parser.add_argument("--close-popup", action="store_true", help="close the invoking popup before switching")
    parser.add_argument("--list", action="store_true", help="print matching pane IDs without switching")
    parser.add_argument("uuid", nargs="?")
    args = parser.parse_args()
    if args.list and args.uuid is None:
        parser.error("--list requires a session UUID")
    finder = Finder(args.tmux, args.socket)
    interactive = args.uuid is None
    if interactive:
        print()
    try:
        while True:
            try:
                value = input(" Session UUID: ") if interactive else args.uuid
                if interactive and not value.strip():
                    return 0
                session_id = normalize_id(value)
                panes = finder.find(session_id)
                if args.list:
                    print(json.dumps(panes))
                    return 0
                if not panes:
                    raise ValueError(
                        "No open Codex pane found. Restore its footer or widen the pane and try again."
                    )
                pane = choose(finder, panes)
                if pane is not None:
                    finder.focus(pane, session_id, args.client, args.close_popup)
                return 0
            except (ValueError, RuntimeError, OSError, subprocess.TimeoutExpired) as error:
                print(str(error), file=sys.stderr)
                if not interactive:
                    return 1
    except (EOFError, KeyboardInterrupt):
        return 0


if __name__ == "__main__":
    sys.exit(main())
