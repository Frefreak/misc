#!/usr/bin/env -S uv run --script
"""Cross-platform desktop notification for Claude Code hooks.

Reads the hook JSON payload on stdin. Dispatches on hook_event_name:
  - Notification         -> show a notification (grouped by session, replaces prior)
  - Stop / SubagentStop  -> clear the notification for that session
"""

from __future__ import annotations

import json
import os
import platform
import shutil
import subprocess
import sys
import time
from pathlib import Path

HOOKS_DIR = Path.home() / ".claude" / "hooks"
IDLE_AUTOCLEAR_SECONDS = 10
# No hook fires when a permission prompt is approved -- the next one is PostToolUse,
# at tool completion -- so notifications are closed by watching for the keystroke that
# answered them. The cap bounds sessions that die at the prompt, and non-tmux ones.
MAX_LIFETIME_SECONDS = 300
WATCH_POLL_SECONDS = 1

_POPEN_KWARGS = {
    "stdin": subprocess.DEVNULL,
    "stdout": subprocess.DEVNULL,
    "stderr": subprocess.DEVNULL,
    "start_new_session": True,
}


def fire_and_forget(args: list[str]) -> None:
    """Spawn detached; do not wait. Returns immediately."""
    try:
        subprocess.Popen(args, **_POPEN_KWARGS)
    except (OSError, subprocess.SubprocessError):
        pass


def read_payload() -> tuple[dict, str]:
    raw = sys.stdin.read()
    try:
        data = json.loads(raw) if raw.strip() else {}
    except json.JSONDecodeError:
        data = {}
    event = data.get("hook_event_name") or "unknown"
    return data, event


def tmux_info() -> tuple[str, str, str]:
    """(pane location, window name, session name); all empty when not under tmux."""
    pane = os.environ.get("TMUX_PANE")
    if not (os.environ.get("TMUX") and pane and shutil.which("tmux")):
        return "", "", ""
    fmt = "#{session_name}:#{window_index}.#{pane_index}\t#{window_name}\t#{session_name}"
    try:
        r = subprocess.run(
            ["tmux", "display-message", "-p", "-t", pane, fmt],
            capture_output=True, text=True, timeout=2,
        )
    except (OSError, subprocess.SubprocessError):
        return "", "", ""
    if r.returncode != 0:
        return "", "", ""
    parts = (r.stdout.strip().split("\t") + ["", "", ""])[:3]
    return parts[0], parts[1], parts[2]


def tmux_client_activity(session: str) -> int | None:
    """Latest input time (epoch secs) across clients attached to `session`.

    Tracks keystrokes only: pane output moves #{window_activity}, not this.
    """
    if not (session and shutil.which("tmux")):
        return None
    try:
        r = subprocess.run(
            ["tmux", "list-clients", "-t", session, "-F", "#{client_activity}"],
            capture_output=True, text=True, timeout=2,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    stamps = [int(s) for s in r.stdout.split() if s.isdigit()]
    return max(stamps) if stamps else None


def build_title_subtitle(payload: dict, loc: str, win: str) -> tuple[str, str]:
    cwd = payload.get("cwd") or ""
    project = Path(cwd).name if cwd else ""
    title = f"Claude Code · {project}" if project else "Claude Code"

    if loc:
        subtitle = f"{loc} [{win}]" if win else loc
    else:
        subtitle = ""
    return title, subtitle


def autoclear_seconds(ntype: str) -> int:
    return IDLE_AUTOCLEAR_SECONDS if ntype == "idle_prompt" else MAX_LIFETIME_SECONDS


def schedule_autoclear(group: str, session: str, seconds: int) -> None:
    """Spawn a detached watcher that closes this notification once answered."""
    baseline = tmux_client_activity(session)
    fire_and_forget([
        str(Path(__file__).resolve()), "--watch",
        group, session, "" if baseline is None else str(baseline), str(seconds),
    ])


def watch(group: str, session: str, baseline: str, seconds: str) -> int:
    """Close once the user types in the session's tmux client, or at the cap."""
    deadline = time.monotonic() + float(seconds)
    base = int(baseline) if baseline else None
    while time.monotonic() < deadline:
        time.sleep(WATCH_POLL_SECONDS)
        if base is None:
            continue
        current = tmux_client_activity(session)
        if current is not None and current > base:
            break
    clear(group)
    return 0


def clear(group: str) -> None:
    system = platform.system()
    if system == "Darwin":
        clear_macos(group)
    elif system == "Linux":
        clear_linux(group)


def show_macos(title: str, subtitle: str, message: str, group: str, session: str, ntype: str) -> None:
    if shutil.which("terminal-notifier"):
        args = [
            "terminal-notifier",
            "-title", title,
            "-message", message,
            "-sound", "Glass",
            "-ignoreDnD",
            "-group", group,
        ]
        if subtitle:
            args += ["-subtitle", subtitle]
        fire_and_forget(args)
        schedule_autoclear(group, session, autoclear_seconds(ntype))
        return
    if shutil.which("osascript"):
        full_title = f"{title} — {subtitle}" if subtitle else title
        script = (
            f"display notification {json.dumps(message)} "
            f"with title {json.dumps(full_title)} sound name \"Glass\""
        )
        fire_and_forget(["osascript", "-e", script])


def clear_macos(group: str) -> None:
    if shutil.which("terminal-notifier"):
        fire_and_forget(["terminal-notifier", "-remove", group])


def linux_id_path(group: str) -> Path:
    return HOOKS_DIR / f".notify-id-{group}"


def show_linux(title: str, subtitle: str, message: str, group: str, session: str, urgency: str, ntype: str) -> None:
    if not shutil.which("notify-send"):
        return
    body = f"{message}\n{subtitle}" if subtitle else message
    # --expire-time is not used: the daemon ignores it while notifications are
    # inhibited, and never expires critical urgency. The watcher closes instead.
    args = ["notify-send", "--print-id", "--app-name=Claude Code", f"--urgency={urgency}"]
    id_file = linux_id_path(group)
    if id_file.exists():
        prev = id_file.read_text().strip()
        if prev:
            args.append(f"--replace-id={prev}")
    args += [title, body]
    # notify-send needs to be awaited to capture --print-id for replacement.
    r = subprocess.run(args, capture_output=True, text=True)
    nid = r.stdout.strip()
    if not nid:
        return
    try:
        id_file.write_text(nid)
    except OSError:
        return
    schedule_autoclear(group, session, autoclear_seconds(ntype))


def clear_linux(group: str) -> None:
    id_file = linux_id_path(group)
    if not id_file.exists():
        return
    nid = id_file.read_text().strip()
    if nid and shutil.which("gdbus"):
        fire_and_forget([
            "gdbus", "call", "--session",
            "--dest=org.freedesktop.Notifications",
            "--object-path=/org/freedesktop/Notifications",
            "--method=org.freedesktop.Notifications.CloseNotification", nid,
        ])
    id_file.unlink(missing_ok=True)


def _permission_message(tool_name: str, tool_input: dict) -> str:
    """Build a human-friendly message for a PermissionRequest payload."""
    # AskUserQuestion: Claude is asking the user a question — not "allow" semantics.
    if tool_name == "AskUserQuestion":
        qs = tool_input.get("questions") or []
        if qs and isinstance(qs, list):
            first = qs[0] if isinstance(qs[0], dict) else {}
            q = first.get("question") or first.get("header") or ""
            if q:
                return f"Question: {q[:120]}"
        return "Claude is asking a question"
    # Standard tool-execution gates: "Allow Bash: <command>" etc.
    for key in ("command", "file_path", "url", "pattern", "path"):
        v = tool_input.get(key)
        if isinstance(v, str) and v:
            return f"Allow {tool_name}: {v[:100]}"
    return f"Allow {tool_name}"


def main() -> int:
    if len(sys.argv) > 1 and sys.argv[1] == "--watch":
        return watch(*sys.argv[2:6])

    payload, event = read_payload()
    group = payload.get("session_id") or "claude-code"
    system = platform.system()

    clear_events = {
        "Stop", "SubagentStop",
        "PostToolUse", "PostToolUseFailure",
        "PermissionDenied",
        "UserPromptSubmit",
    }
    if event in clear_events:
        clear(group)
        return 0

    loc, win, session = tmux_info()
    title, subtitle = build_title_subtitle(payload, loc, win)
    if event == "PermissionRequest":
        tool_name = payload.get("tool_name") or "tool"
        tool_input = payload.get("tool_input") or {}
        message = _permission_message(tool_name, tool_input)
        ntype = "permission_prompt"
    else:
        message = payload.get("message") or "Needs your attention"
        ntype = payload.get("notification_type") or ""
    urgency = "critical" if ntype == "permission_prompt" else "normal"

    if system == "Darwin":
        show_macos(title, subtitle, message, group, session, ntype)
    elif system == "Linux":
        show_linux(title, subtitle, message, group, session, urgency, ntype)
    return 0


if __name__ == "__main__":
    sys.exit(main())
