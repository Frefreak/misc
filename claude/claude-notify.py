#!/Users/even/.venv/bin/python
"""Cross-platform desktop notification for Claude Code hooks.

Reads the hook JSON payload on stdin. Dispatches on hook_event_name:
  - Notification         -> show a notification (grouped by session, replaces prior)
  - Stop / SubagentStop  -> clear the notification for that session
"""

from __future__ import annotations

import json
import os
import platform
import shlex
import shutil
import subprocess
import sys
from pathlib import Path

HOOKS_DIR = Path.home() / ".claude" / "hooks"
IDLE_AUTOCLEAR_SECONDS = 10

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


def tmux_info() -> tuple[str, str]:
    pane = os.environ.get("TMUX_PANE")
    if not (os.environ.get("TMUX") and pane and shutil.which("tmux")):
        return "", ""

    def fetch(fmt: str) -> str:
        try:
            r = subprocess.run(
                ["tmux", "display-message", "-p", "-t", pane, fmt],
                capture_output=True, text=True, timeout=2,
            )
            return r.stdout.strip() if r.returncode == 0 else ""
        except (OSError, subprocess.SubprocessError):
            return ""

    return (
        fetch("#{session_name}:#{window_index}.#{pane_index}"),
        fetch("#{window_name}"),
    )


def build_title_subtitle(payload: dict) -> tuple[str, str]:
    cwd = payload.get("cwd") or ""
    project = Path(cwd).name if cwd else ""
    title = f"Claude Code · {project}" if project else "Claude Code"

    loc, win = tmux_info()
    if loc:
        subtitle = f"{loc} [{win}]" if win else loc
    else:
        subtitle = ""
    return title, subtitle


def schedule_macos_autoclear(group: str, seconds: int) -> None:
    """Spawn a detached `sleep N && terminal-notifier -remove <group>`."""
    if not shutil.which("terminal-notifier"):
        return
    cmd = f"sleep {seconds} && terminal-notifier -remove {shlex.quote(group)}"
    subprocess.Popen(
        ["bash", "-c", cmd],
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        start_new_session=True,
    )


def show_macos(title: str, subtitle: str, message: str, group: str, ntype: str) -> None:
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
        if ntype == "idle_prompt":
            schedule_macos_autoclear(group, IDLE_AUTOCLEAR_SECONDS)
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


def show_linux(title: str, subtitle: str, message: str, group: str, urgency: str, ntype: str) -> None:
    if not shutil.which("notify-send"):
        return
    body = f"{message}\n{subtitle}" if subtitle else message
    args = ["notify-send", "--print-id", "--app-name=Claude Code", f"--urgency={urgency}"]
    if ntype == "idle_prompt":
        args.append(f"--expire-time={IDLE_AUTOCLEAR_SECONDS * 1000}")
    id_file = linux_id_path(group)
    if id_file.exists():
        prev = id_file.read_text().strip()
        if prev:
            args.append(f"--replace-id={prev}")
    args += [title, body]
    # notify-send needs to be awaited to capture --print-id for replacement.
    r = subprocess.run(args, capture_output=True, text=True)
    nid = r.stdout.strip()
    if nid:
        try:
            id_file.write_text(nid)
        except OSError:
            pass


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
    payload, event = read_payload()
    group = payload.get("session_id") or "claude-code"
    system = platform.system()

    clear_events = {
        "Stop", "SubagentStop",
        "PreToolUse",
        "PostToolUse", "PostToolUseFailure",
        "PermissionDenied",
        "UserPromptSubmit",
    }
    if event in clear_events:
        if system == "Darwin":
            clear_macos(group)
        elif system == "Linux":
            clear_linux(group)
        return 0

    title, subtitle = build_title_subtitle(payload)
    if event == "PermissionRequest":
        tool_name = payload.get("tool_name") or "tool"
        tool_input = payload.get("tool_input") or {}
        message = _permission_message(tool_name, tool_input)
        ntype = "permission_prompt"
    else:
        message = payload.get("message") or "Needs your attention"
        ntype = payload.get("notification_type") or ""
    urgency = "critical" if ntype in ("permission_prompt", "idle_prompt") else "normal"

    if system == "Darwin":
        show_macos(title, subtitle, message, group, ntype)
    elif system == "Linux":
        show_linux(title, subtitle, message, group, urgency, ntype)
    return 0


if __name__ == "__main__":
    sys.exit(main())
