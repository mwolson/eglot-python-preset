#!/usr/bin/env python3

import json
import sys


def send(message):
    data = json.dumps(message).encode("utf-8")
    sys.stdout.buffer.write(
        f"Content-Length: {len(data)}\r\n\r\n".encode("ascii") + data
    )
    sys.stdout.buffer.flush()


def read_message():
    content_length = None

    while True:
        line = sys.stdin.buffer.readline()
        if not line:
            return None
        if line == b"\r\n":
            break
        if line.lower().startswith(b"content-length:"):
            content_length = int(line.split(b":", 1)[1].strip())

    if content_length is None:
        raise RuntimeError("Missing Content-Length header")

    payload = sys.stdin.buffer.read(content_length)
    if not payload:
        return None
    return json.loads(payload.decode("utf-8"))


def main():
    args = sys.argv[1:]
    program = None
    if len(args) >= 2 and args[0] == "--server-name":
        program = args[1].lower()
        args = args[2:]
    diagnostic_code = "FLAG_PASSED" if "--isolated" in args else "FLAG_MISSING"

    while True:
        message = read_message()
        if message is None:
            return 0

        method = message.get("method")

        if method == "initialize":
            init_options = (
                message.get("params", {}).get("initializationOptions") or {}
            )
            if program == "ty":
                config = init_options.get("configuration", {})
                env = config.get("environment", {})
                diagnostic_code = (
                    "INIT_OPTIONS_PRESENT"
                    if env.get("python") and env.get("root")
                    else "INIT_OPTIONS_MISSING"
                )
            elif "configuration" in init_options:
                diagnostic_code = "INIT_OPTIONS_LEAKED"
            if "rass" in init_options:
                diagnostic_code = "RASS_KEY_LEAKED"
            send(
                {
                    "jsonrpc": "2.0",
                    "id": message["id"],
                    "result": {"capabilities": {"textDocumentSync": 1}},
                }
            )
            continue

        if method == "textDocument/didOpen":
            send(
                {
                    "jsonrpc": "2.0",
                    "method": "textDocument/publishDiagnostics",
                    "params": {
                        "uri": message["params"]["textDocument"]["uri"],
                        "diagnostics": [
                            {
                                "range": {
                                    "start": {"line": 0, "character": 0},
                                    "end": {"line": 0, "character": 1},
                                },
                                "severity": 2,
                                "source": "fake-ruff",
                                "code": diagnostic_code,
                                "message": diagnostic_code,
                            }
                        ],
                    },
                }
            )
            continue

        if method == "shutdown":
            send({"jsonrpc": "2.0", "id": message["id"], "result": None})
            continue

        if method == "exit":
            return 0


if __name__ == "__main__":
    raise SystemExit(main())
