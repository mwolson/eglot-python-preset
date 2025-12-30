# eglot-python-preset

<!-- TODO: Uncomment when available on MELPA
[![MELPA](https://melpa.org/packages/eglot-python-preset-badge.svg)](https://melpa.org/#/eglot-python-preset)
-->

Configures Python LSP support for Emacs using
[Eglot](https://github.com/joaotavora/eglot), including support for
[PEP-723](https://peps.python.org/pep-0723/) script metadata and automatic
project detection.

This package configures Eglot to work with Python files using either
[ty](https://github.com/astral-sh/ty) or
[basedpyright](https://github.com/DetachHead/basedpyright) as the language
server. It automatically handles environment synchronization for
[uv](https://github.com/astral-sh/uv)-managed scripts with inline dependencies.

## Prerequisites

- Emacs 30.2 or later
- [uv](https://docs.astral.sh/uv/) - fast Python package installer and resolver
- One of the following language servers:
  - [ty](https://github.com/astral-sh/ty) (>= v0.0.8) - Astral's new Python type
    checker
  - [basedpyright](https://github.com/DetachHead/basedpyright) - fork of pyright
    with additional features

## Installation

Choose one of the following ways to install. After that, opening Python files
will automatically start the LSP server using Eglot.

> **Note**: This package is not yet available on MELPA. For now, install
> manually.

<!-- TODO: Uncomment when available on MELPA
### From MELPA

```elisp
(use-package eglot-python-preset
  :ensure t
  :after eglot
  :config
  (setopt eglot-python-preset-lsp-server 'ty) ; or 'basedpyright
  (eglot-python-preset-setup))
```
-->

### With use-package (manual)

Clone this repository:

```bash
mkdir -p ~/devel
git clone https://github.com/mwolson/eglot-python-preset ~/devel/eglot-python-preset
```

Add to your Emacs configuration:

```elisp
(use-package eglot-python-preset
  :load-path "~/devel/eglot-python-preset"
  :config
  (setopt eglot-python-preset-lsp-server 'ty) ; or 'basedpyright
  (eglot-python-preset-setup))
```

### With require

Clone this repository:

```bash
mkdir -p ~/devel
git clone https://github.com/mwolson/eglot-python-preset ~/devel/eglot-python-preset
```

Add to your Emacs configuration:

```elisp
(add-to-list 'load-path (expand-file-name "~/devel/eglot-python-preset"))
(require 'eglot-python-preset)
(setopt eglot-python-preset-lsp-server 'ty) ; or 'basedpyright
(eglot-python-preset-setup)
```

## Usage

### Standard Python Projects

For standard Python projects (those with `pyproject.toml` or
`requirements.txt`), the package automatically detects the project root and
starts Eglot with appropriate configuration.

### PEP-723 Scripts

[PEP-723](https://peps.python.org/pep-0723/) allows embedding dependency
metadata directly in Python scripts using special comments:

```python
# /// script
# requires-python = ">=3.10"
# dependencies = [
#     "requests>=2.31.0",
# ]
# ///

import requests

response = requests.get("https://example.com")
print(response.status_code)
```

When you open a file containing PEP-723 metadata:

1. The package detects the script metadata automatically
2. It locates or allows you to manually create an isolated environment via `uv`
3. The LSP server is configured to use that environment for type checking

### Commands

- **`M-x eglot-python-preset-sync-environment`** - Sync dependencies for the
  current PEP-723 script using `uv sync --script`, then restart Eglot. Use this
  after adding or modifying dependencies in your script's metadata block.

- **`M-x eglot-python-preset-remove-environment`** - Remove the cached uv
  environment for the current PEP-723 script. Useful when you want to force a
  clean reinstall of dependencies or troubleshoot environment issues. After
  removal, run `eglot-python-preset-sync-environment` to recreate it.

- **`M-x eglot-python-preset-run-script`** - Run the current PEP-723 script
  using `uv run`. Opens a compilation buffer with the output.

## Configuration

### `eglot-python-preset-lsp-server`

Choose which language server to use:

```elisp
(setopt eglot-python-preset-lsp-server 'ty)         ; default
;; or
(setopt eglot-python-preset-lsp-server 'basedpyright)
```

### `eglot-python-preset-python-project-markers`

Files that indicate a Python project root:

```elisp
(setopt eglot-python-preset-python-project-markers
        '("pyproject.toml" "requirements.txt"))  ; default
```

### `eglot-python-preset-workspace-config-plist`

Additional workspace configuration to send to the LSP server. This plist is
merged into the `workspace/configuration` response. Currently only used with
basedpyright.

Example to disable auto-import completions and set type checking mode:

```elisp
(setopt eglot-python-preset-workspace-config-plist
        '(:basedpyright.analysis
          (:autoImportCompletions :json-false
           :typeCheckingMode "basic")))
```

## Notes

- The package uses `uv` for all Python environment management. Ensure `uv` is
  installed and in your PATH.
- For PEP-723 scripts, environments are cached by `uv` and shared across
  sessions.
- If you see a warning about the environment not being synced, run
  `M-x eglot-python-preset-sync-environment`.
