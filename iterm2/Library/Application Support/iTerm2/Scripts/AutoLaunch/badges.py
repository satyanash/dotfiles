#!/usr/bin/env python3
"""iTerm2 daemon that sets a tab emoji based on the current working directory."""

import asyncio
import iterm2
import logging
import os

logging.basicConfig(level=logging.DEBUG, format="%(asctime)s [%(levelname)s] %(message)s")
log: logging.Logger = logging.getLogger("emoji_tabs")

# Directory patterns and their emojis. First match wins.
PREFIX_PATTERNS: list[tuple[str, str]] = [
    ("~/Code",      "💻"),
    ("~/Documents", "📄"),
    ("~/Downloads", "📥"),
    ("~/.config",   "⚙️"),
    ("~/Desktop",   "🖥️"),
    ("~",           "🏠"),
]

GLOB_PATTERNS: list[tuple[str, str]] = [
    ("catto",    "🐱"),
    ("elephanto", "🐘"),
    ("doggo",    "🐶"),
]

EXPANDED_PREFIXES: list[tuple[str, str]] = [(os.path.expanduser(p), e) for p, e in PREFIX_PATTERNS]


def emoji_for_path(path: str | None) -> str:
    if path:
        for substring, emoji in GLOB_PATTERNS:
            if substring in path:
                return emoji
        for prefix, emoji in EXPANDED_PREFIXES:
            if path.startswith(prefix):
                return emoji
    return ""


async def update_session(session: iterm2.Session, path: str | None = None) -> None:
    if path is None:
        path = await session.async_get_variable("path")
    emoji: str = emoji_for_path(path)
    log.debug("session %s: path=%s emoji=%s", session.session_id, path, emoji or "(none)")
    if emoji:
        await session.async_set_variable("user.badge", emoji)


async def monitor_session(connection: iterm2.Connection, session: iterm2.Session) -> None:
    """Monitor a single session's path variable and update its emoji."""
    log.info("watching session %s", session.session_id)
    try:
        async with iterm2.VariableMonitor(
            connection,
            iterm2.VariableScopes.SESSION,
            "path",
            session.session_id,
        ) as mon:
            while True:
                new_path: str = await mon.async_get()
                await update_session(session, path=new_path)
    except Exception as e:
        log.info("stopped watching session %s: %s", session.session_id, e)


async def main(connection: iterm2.Connection) -> None:
    app: iterm2.App = await iterm2.async_get_app(connection)
    log.info("emoji_tabs started, prefixes: %s, globs: %s", PREFIX_PATTERNS, GLOB_PATTERNS)

    # Update and monitor all existing sessions
    for window in app.terminal_windows:
        for tab in window.tabs:
            for session in tab.sessions:
                log.info("initial update for session %s", session.session_id)
                await update_session(session)
                asyncio.create_task(monitor_session(connection, session))

    # Watch for new sessions and start monitoring them
    log.info("waiting for new sessions")
    async with iterm2.NewSessionMonitor(connection) as mon:
        while True:
            session_id: str = await mon.async_get()
            log.info("new session %s", session_id)
            session: iterm2.Session | None = app.get_session_by_id(session_id)
            if session:
                await update_session(session)
                asyncio.create_task(monitor_session(connection, session))
            else:
                log.warning("could not find new session %s", session_id)


iterm2.run_forever(main)
