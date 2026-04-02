# eglot-python-preset

[![MELPA](https://melpa.org/packages/eglot-python-preset-badge.svg)](https://melpa.org/#/eglot-python-preset)
![Made for GNU Emacs](assets/badges/made-for-gnu-emacs.svg)

Configures Python LSP support for Emacs using
[Eglot](https://github.com/joaotavora/eglot), including support for
[PEP-723](https://peps.python.org/pep-0723/) script metadata and automatic
project detection.

This package configures Eglot to work with Python files using
[ty](https://github.com/astral-sh/ty),
[basedpyright](https://github.com/DetachHead/basedpyright), or
[rassumfrassum](https://github.com/joaotavora/rassumfrassum) as the language
server frontend. It automatically handles environment synchronization for
[uv](https://github.com/astral-sh/uv)-managed scripts with inline dependencies.

See also
[eglot-typescript-preset](https://github.com/mwolson/eglot-typescript-preset)
for TypeScript, JavaScript, CSS, Astro, Vue, and Svelte support.

## Prerequisites

- Emacs 30.2 or later
- [uv](https://docs.astral.sh/uv/) - fast Python package installer and resolver
- One of the following language servers:
  - [ty](https://github.com/astral-sh/ty) (>= v0.0.8) - Astral's new Python type
    checker. It can be installed globally or in a project-root `.venv`.
  - [basedpyright](https://github.com/DetachHead/basedpyright) - fork of pyright
    with additional features. It can be installed globally or in a project-root
    `.venv`.
  - [rassumfrassum](https://github.com/joaotavora/rassumfrassum) (>= v0.3.3) -
    optional stdio multiplexer for combining multiple Python tools.

## Installation

Choose one of the following ways to install. After that, opening Python files
will automatically start the LSP server using Eglot.

### Using package-vc (recommended)

For Emacs 30.2+, use the built-in `package-vc` via `use-package`:

```elisp
(use-package eglot-python-preset
  :vc (:url "https://github.com/mwolson/eglot-python-preset"
       :main-file "eglot-python-preset.el"))
```

This will download and install the package automatically if it is not already
installed. By default, `:vc` installs the latest release rather than the latest
commit. `ty` is used as the default language server.

To track the development version instead, use `:rev :newest`:

```elisp
(use-package eglot-python-preset
  :vc (:url "https://github.com/mwolson/eglot-python-preset"
       :main-file "eglot-python-preset.el"
       :rev :newest))
```

The package sets up Eglot integration automatically when Eglot loads, so no
explicit setup call is needed.

If you're interested in trying out some alpha software to improve `package-vc`
further, take a look at [vcupp](https://github.com/mwolson/vcupp).

### Manual installation

Clone this repository:

```bash
mkdir -p ~/devel
git clone https://github.com/mwolson/eglot-python-preset ~/devel/eglot-python-preset
```

Then add it to your Emacs configuration:

```elisp
(add-to-list 'load-path (expand-file-name "~/devel/eglot-python-preset"))
(require 'eglot-python-preset)
(setopt eglot-python-preset-lsp-server 'ty) ; or 'basedpyright or 'rass
```

### From MELPA

```elisp
(use-package eglot-python-preset
  :ensure t
  :custom
  (eglot-python-preset-lsp-server 'ty)) ; or 'basedpyright or 'rass
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

### `eglot-python-preset-auto-setup`

Controls whether Eglot integration is set up automatically when Eglot loads
(default: `t`). Set to `nil` before the package loads to suppress automatic
setup and call `eglot-python-preset-setup` manually instead:

```elisp
(use-package eglot-python-preset
  :vc (:url "https://github.com/mwolson/eglot-python-preset"
       :main-file "eglot-python-preset.el")
  :custom
  (eglot-python-preset-auto-setup nil)
  :config
  (eglot-python-preset-setup))
```

### `eglot-python-preset-lsp-server`

Choose which language server to use:

```elisp
(setopt eglot-python-preset-lsp-server 'ty)         ; default
;; or
(setopt eglot-python-preset-lsp-server 'basedpyright)
;; or
(setopt eglot-python-preset-lsp-server 'rass)
```

### `eglot-python-preset-rass-tools`

When using the `rass` backend, this list controls the generated preset.
Supported symbolic tools get local `.venv` executable resolution and special
handling for PEP-723 where available.

Literal commands in a vector are passed through to `rass`:

```elisp
(setopt eglot-python-preset-rass-tools
        '(ty
          ruff
          ["custom-lsp" "--stdio"]))
```

If you want to pass arguments to a built-in tool, spell that entry out as a
literal vector. For example, Ruff supports top-level `--isolated` before the
`server` subcommand, which can be useful when you want to ignore ambient Ruff
configuration files:

```elisp
(setopt eglot-python-preset-rass-tools
        '(ty
          ["ruff" "--isolated" "server"]))
```

### `eglot-python-preset-rass-command`

If you want to bypass the generated preset entirely, set an exact `rass` command
vector. When this is non-nil, it is used verbatim and
`eglot-python-preset-rass-tools` is ignored:

```elisp
(setopt eglot-python-preset-rass-command ["rass" "python"])
```

Or with a custom preset path:

```elisp
(setopt eglot-python-preset-rass-command
        ["rass" "/path/to/custom-preset.py"])
```

Note that this will remove support for PEP-723 scripts unless the preset is
updated to handle it.

### `eglot-python-preset-python-modes`

Major modes for Python files:

```elisp
(setopt eglot-python-preset-python-modes
        '(python-mode python-ts-mode))  ; default
```

### `eglot-python-preset-python-project-markers`

Files that indicate a Python project root:

```elisp
(setopt eglot-python-preset-python-project-markers
        '("pyproject.toml" "requirements.txt"))  ; default
```

### `eglot-python-preset-rass-max-contextual-presets`

Contextual `rass` presets are the ones that embed per-script or project-local
state, such as PEP-723 Ty configuration or a project-local `.venv` executable
path. Older contextual presets are pruned automatically to keep the generated
directory from growing without bound:

```elisp
(setopt eglot-python-preset-rass-max-contextual-presets 50) ; default
```

### Workspace Configuration (basedpyright)

To customize basedpyright settings, set `eglot-workspace-configuration`. Your
settings will be merged with PEP-723 script configurations (which add the Python
interpreter path).

Example to disable auto-import completions and set type checking mode:

```elisp
;; With use-package
(use-package eglot-python-preset
  :vc (:url "https://github.com/mwolson/eglot-python-preset"
       :main-file "eglot-python-preset.el")
  :custom
  (eglot-python-preset-lsp-server 'basedpyright)
  (eglot-workspace-configuration
   '(:basedpyright.analysis
     (:autoImportCompletions :json-false
      :typeCheckingMode "basic"))))

;; Or manually (can be spread across multiple init.el sections)
(setopt eglot-python-preset-lsp-server 'basedpyright)
(setopt eglot-workspace-configuration
        (plist-put eglot-workspace-configuration
                   :basedpyright.analysis
                   '(:autoImportCompletions :json-false
                     :typeCheckingMode "basic")))
```

### Per-project Configuration

Some projects may need different settings than your global defaults. There are
two approaches, depending on whether you want the settings stored in the project
itself.

#### Using `.dir-locals.el` (project-local file)

Create a `.dir-locals.el` file in the project root. The following variables are
recognized as safe with appropriate values, so Emacs will apply them without
prompting:

```elisp
;;; .dir-locals.el
((python-ts-mode
  . ((eglot-python-preset-lsp-server . basedpyright)
     (eglot-python-preset-rass-tools . (ty ruff)))))
```

- `eglot-python-preset-lsp-server` accepts `ty`, `basedpyright`, or `rass`.
- `eglot-python-preset-rass-tools` accepts lists of known tool symbols (`ty`,
  `ruff`, `basedpyright`).
- `eglot-python-preset-python-modes` accepts lists of symbols.
- `eglot-python-preset-python-project-markers` accepts lists of filename
  strings.

#### Using `dir-locals-set-directory-class` (init file, no project changes)

If you prefer not to add Emacs-specific files to the project, configure
per-directory settings from your init file instead:

```elisp
(dir-locals-set-class-variables
 'my-project-x
 '((python-ts-mode
    . ((eglot-python-preset-lsp-server . basedpyright)))))

(dir-locals-set-directory-class
 (expand-file-name "~/devel/project-x") 'my-project-x)
```

This uses the built-in Emacs directory-class mechanism. The settings take effect
whenever you visit files under that directory, without any files added to the
project.

## Troubleshooting

- Eglot publishes diagnostics through Flymake. If you are using Flycheck, you
  will need separate bridge or integration configuration in your Emacs setup.
- If `ty` or `basedpyright-langserver` is installed only in a project-local
  `.venv`, make sure you are using v0.3.0 or later so this package can prefer
  that executable automatically.
- If you use the `rass` backend, the package generates a preset under your Emacs
  directory and updates it as needed. Context-free presets are reused across
  buffers, while PEP-723 and project-local `.venv` cases keep separate generated
  files when the preset content depends on that local context.

## Notes

- The package uses `uv` for all Python environment management. Ensure `uv` is
  installed and in your PATH.
- For standard Python projects, the package prefers `ty` or
  `basedpyright-langserver` from a project-root `.venv` and otherwise falls back
  to PATH. The same resolution is used for supported tools in generated `rass`
  presets.
- For PEP-723 scripts, environments are cached by `uv` and shared across
  sessions.
- If you see a warning about the environment not being synced, run
  `M-x eglot-python-preset-sync-environment`.
- When using the `rass` backend, the package enables streaming diagnostics so
  that LSP servers using pull diagnostics (like some linters) work correctly
  with Eglot's push-based diagnostics model.
