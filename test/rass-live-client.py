#!/usr/bin/env python3

import argparse
import json
import queue
import subprocess
import threading
import time
from pathlib import Path


def send(proc, message):
    data = json.dumps(message).encode("utf-8")
    proc.stdin.write(f"Content-Length: {len(data)}\r\n\r\n".encode("ascii") + data)
    proc.stdin.flush()


def read_message(proc):
    content_length = None
    while True:
        line = proc.stdout.readline()
        if not line:
            return None
        if line == b"\r\n":
            break
        if line.lower().startswith(b"content-length:"):
            content_length = int(line.split(b":", 1)[1].strip())

    if content_length is None:
        raise RuntimeError("Missing Content-Length header")

    payload = proc.stdout.read(content_length)
    if not payload:
        return None
    return json.loads(payload.decode("utf-8"))


def start_message_reader(proc):
    message_queue = queue.Queue()

    def read_loop():
        try:
            while True:
                message = read_message(proc)
                message_queue.put(message)
                if message is None:
                    return
        except Exception as exc:
            message_queue.put(exc)

    thread = threading.Thread(target=read_loop, daemon=True)
    thread.start()
    return message_queue, thread


def wait_for_message(message_queue, timeout):
    try:
        message = message_queue.get(timeout=timeout)
    except queue.Empty:
        return None
    if isinstance(message, Exception):
        raise message
    return message


def request(proc, next_id, method, params):
    send(proc, {"jsonrpc": "2.0", "id": next_id, "method": method, "params": params})
    return next_id + 1


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("preset")
    parser.add_argument("file")
    parser.add_argument("--server", default="rass")
    parser.add_argument("--timeout", type=float, default=8.0)
    args = parser.parse_args()

    file_path = Path(args.file).resolve()
    uri = file_path.as_uri()
    root_path = file_path.parent

    proc = subprocess.Popen(
        [args.server, args.preset],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )

    next_id = 1
    workspace_sections = []
    diagnostic_sources = []
    diagnostic_codes = []
    diagnostics = []
    initialized = False
    message_queue, reader_thread = start_message_reader(proc)

    try:
        next_id = request(
            proc,
            next_id,
            "initialize",
            {
                "processId": None,
                "rootUri": root_path.as_uri(),
                "capabilities": {
                    "workspace": {"configuration": True},
                    "textDocument": {"publishDiagnostics": {}},
                },
                "workspaceFolders": [
                    {"uri": root_path.as_uri(), "name": root_path.name or "workspace"}
                ],
            },
        )

        content = file_path.read_text(encoding="utf-8")
        sent_open = False
        deadline = time.monotonic() + args.timeout
        last_message_at = time.monotonic()
        saw_diagnostics = False

        while time.monotonic() < deadline:
            if proc.poll() is not None:
                break

            message = wait_for_message(
                message_queue, max(0.0, deadline - time.monotonic())
            )
            if message is None:
                if initialized and sent_open:
                    idle = time.monotonic() - last_message_at
                    if saw_diagnostics and idle > 0.2:
                        break
                    if idle > 0.5:
                        break
                continue

            last_message_at = time.monotonic()
            method = message.get("method")

            if "id" in message and message.get("id") == 1 and "result" in message:
                initialized = True
                send(proc, {"jsonrpc": "2.0", "method": "initialized", "params": {}})
                send(
                    proc,
                    {
                        "jsonrpc": "2.0",
                        "method": "textDocument/didOpen",
                        "params": {
                            "textDocument": {
                                "uri": uri,
                                "languageId": "python",
                                "version": 1,
                                "text": content,
                            }
                        },
                    },
                )
                sent_open = True
                continue

            if method == "workspace/configuration":
                for item in message.get("params", {}).get("items", []):
                    workspace_sections.append(item.get("section", ""))
                send(
                    proc,
                    {
                        "jsonrpc": "2.0",
                        "id": message["id"],
                        "result": [
                            None for _ in message.get("params", {}).get("items", [])
                        ],
                    },
                )
                continue

            if method == "textDocument/publishDiagnostics":
                saw_diagnostics = True
                for diagnostic in message.get("params", {}).get("diagnostics", []):
                    source = diagnostic.get("source")
                    code = diagnostic.get("code")
                    if source is not None:
                        diagnostic_sources.append(str(source))
                    if code is not None:
                        diagnostic_codes.append(str(code))
                    diagnostics.append(
                        {
                            "source": source,
                            "code": code,
                            "message": diagnostic.get("message", ""),
                            "severity": diagnostic.get("severity"),
                        }
                    )
                continue

        next_id = request(proc, next_id, "shutdown", None)
        end_deadline = time.monotonic() + 2.0
        while time.monotonic() < end_deadline:
            message = wait_for_message(
                message_queue, max(0.0, end_deadline - time.monotonic())
            )
            if message is None:
                continue
            if message.get("id") == next_id - 1:
                break
        send(proc, {"jsonrpc": "2.0", "method": "exit", "params": {}})
    finally:
        try:
            proc.terminate()
        except Exception:
            pass
        try:
            proc.wait(timeout=1.0)
        except Exception:
            proc.kill()
        reader_thread.join(timeout=1.0)

    print(
        json.dumps(
            {
                "initialized": initialized,
                "workspaceConfigSections": workspace_sections,
                "diagnosticSources": diagnostic_sources,
                "diagnosticCodes": diagnostic_codes,
                "diagnostics": diagnostics,
            }
        )
    )


if __name__ == "__main__":
    main()
