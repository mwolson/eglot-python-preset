#!/usr/bin/env python3

import asyncio
import importlib.util
import json
import os
import re
import sys
import types
from pathlib import Path


def ensure_rassumfrassum_stubs():
    package = types.ModuleType("rassumfrassum")
    frassum = types.ModuleType("rassumfrassum.frassum")
    json_mod = types.ModuleType("rassumfrassum.json")
    util = types.ModuleType("rassumfrassum.util")

    class LspLogic:
        async def on_client_request(self, method, params, servers):
            return servers

        def process_request(self, method, params, server):
            # Mirrors rass 0.3.4+ handling of the "rass" key.
            init_opts = params.get("initializationOptions")
            if method == "initialize" and init_opts and "rass" in init_opts:
                generic = {k: v for k, v in init_opts.items() if k != "rass"}
                new_opts = generic
                for pattern, overlay in init_opts["rass"].items():
                    if re.search(pattern, server.name):
                        new_opts = dmerge(overlay, generic)
                        break
                params = {**params, "initializationOptions": new_opts}
            return params

    class Server:
        pass

    def dmerge(base, override):
        result = dict(base)
        for key, value in override.items():
            if isinstance(result.get(key), dict) and isinstance(value, dict):
                result[key] = dmerge(result[key], value)
            else:
                result[key] = value
        return result

    def _diff_changes(old, new):
        raise AssertionError("preset should replace rass _diff_changes")

    frassum._diff_changes = _diff_changes
    frassum.LspLogic = LspLogic
    frassum.Server = Server
    json_mod.JSON = dict
    util.dmerge = dmerge

    sys.modules.setdefault("rassumfrassum", package)
    sys.modules["rassumfrassum.frassum"] = frassum
    sys.modules["rassumfrassum.json"] = json_mod
    sys.modules["rassumfrassum.util"] = util


DIFF_CASES = [
    ("one\x0c\ntwo\nthree\n", "one\x0c\ntwo\nTHREE\n"),
    ("one\u2028two\nthree\n", "one\u2028two\nTHREE\n"),
    ("one\x85two\nthree\n", "one\x85two\nTHREE\n"),
    ("one\rtwo\nthree\n", "one\rtwo\nTHREE\n"),
    ("a\r\nb\r\n", "a\r\nB\r\n"),
    ("a\r\nb\n", "a\nb\n"),
    ("a\r\nb\n", "a\rb\n"),
    ("x\r\ny\n", "x\r\r\ny\n"),
    ("a\nb\n", "a\r\nb\n"),
    ("a\r", "a\r\nb"),
    ("cr\rx\nlf", "cr\r\nlf"),
    ("// \U0001F600 x\n", "// \U0001F600! x\n"),
    ("aa", "aaa"),
    ("abc", ""),
    ("", "abc"),
    ("same\n", "same\n"),
]

EOL = re.compile(r"\r\n|\r|\n")


def apply_changes(text, changes):
    """Apply LSP contentChanges the way the specification describes.

    Lines end at LF, CRLF or CR, columns count UTF-16 code units, and a
    column past the end of a line clamps to the end of its content.
    Return None if a change's edges split or form a CRLF pair, which
    servers built on vscode-languageserver-textdocument mishandle.
    """

    def offset(pos):
        starts = [0] + [m.end() for m in EOL.finditer(text)]
        if pos["line"] >= len(starts):
            return len(text)
        index = starts[pos["line"]]
        eol = EOL.search(text, index)
        content_end = eol.start() if eol else len(text)
        units = 0
        while index < content_end and units < pos["character"]:
            units += 2 if ord(text[index]) > 0xFFFF else 1
            index += 1
        return index

    for change in changes:
        start = offset(change["range"]["start"])
        end = offset(change["range"]["end"])
        after = text[:start] + change["text"] + text[end:]
        edges = [
            (text, start),
            (text, end),
            (after, start),
            (after, start + len(change["text"])),
        ]
        if any(i > 0 and t[i - 1 : i + 1] == "\r\n" for t, i in edges):
            return None
        text = after
    return text


def diff_changes_failures(module):
    """Return the DIFF_CASES that _diff_changes does not reproduce."""
    return [
        [old, new]
        for old, new in DIFF_CASES
        if apply_changes(old, module._diff_changes(old, new)) != new
    ]


def load_module(path_str):
    ensure_rassumfrassum_stubs()
    path = Path(path_str)
    spec = importlib.util.spec_from_file_location("generated_rass_preset", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"Unable to load preset module from {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


async def initialize_options_by_server(module):
    """Return the initializationOptions each server receives."""
    servers = []
    for index, command in enumerate(module.SERVERS):
        server = sys.modules["rassumfrassum.frassum"].Server()
        base = os.path.basename(command[0])
        server.name = f"{base}#{index}" if index else base
        servers.append(server)
    logic = module.logic_class().__new__(module.logic_class())
    params = {"initializationOptions": {}}
    await logic.on_client_request("initialize", params, servers)
    return [
        logic.process_request("initialize", params, server).get(
            "initializationOptions"
        )
        for server in servers
    ]


def main():
    module = load_module(sys.argv[1])
    server_kind = {
        "basedpyright": module._server_kind("basedpyright"),
        "basedpyright-langserver": module._server_kind("basedpyright-langserver"),
        "pyrefly": module._server_kind("pyrefly"),
        "ruff": module._server_kind("ruff"),
        "ty": module._server_kind("ty"),
        "unknown": module._server_kind("custom-lsp"),
    }

    request_params = {
        "items": [
            {"section": "ty"},
            {"section": "python"},
            {"section": "ignored"},
        ]
    }
    response_payload = [{"existing": True}]
    module._merge_response_items(
        request_params,
        response_payload,
        ("ty", "python"),
        {"configuration": {"environment": {"python": "/tmp/env/"}}},
    )
    non_list_payload = {"unchanged": True}
    module._merge_response_items(
        request_params,
        non_list_payload,
        ("ty",),
        {"configuration": {"environment": {"python": "/tmp/env/"}}},
    )

    print(
        json.dumps(
            {
                "serverKind": server_kind,
                "diffChangesFailures": diff_changes_failures(module),
                "diffChangesInstalled": sys.modules[
                    "rassumfrassum.frassum"
                ]._diff_changes
                is module._diff_changes,
                "initializeOptionsByServer": asyncio.run(
                    initialize_options_by_server(module)
                ),
                "nonListPayload": non_list_payload,
                "responsePayload": response_payload,
            }
        )
    )


if __name__ == "__main__":
    raise SystemExit(main())
