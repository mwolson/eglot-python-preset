# AGENTS.md

This repository is for an Emacs library that configures Eglot.

## Architecture

For understanding the PEP-723 implementation approach (project detection, advice
on `eglot--workspace-configuration-plist`, per-script server instances), see
`plans/pep-723.md`.

## Planning

Prefer to write plans in the `plans/` directory.

## Dev loop tools

Here are some strategies to obtain a reliable "dev loop" for validating
formatter integrations.

### Running tests

Run tests with:

```sh
bun run test
```

This executes the test suite found in `test/pep-723/test.el`.

### Introspecting Elisp functions/variables from CLI

To check a function's arguments or documentation without starting interactive
Emacs:

- Get argument list (using `eglot` as an example):
  - `emacs -Q --batch --eval "(require 'eglot)" --eval "(princ (help-function-arglist 'eglot))"`
- Get function documentation (using `eglot` as an example):
  - `emacs -Q --batch --eval "(require 'eglot)" --eval "(princ (documentation 'eglot))"`
- Get variable documentation (using `eglot-server-programs` as an example):
  - `emacs -Q --batch --eval "(require 'eglot)" --eval "(princ (documentation-property 'eglot-server-programs 'variable-documentation))"`

### Quick Elisp sanity check

- Check parentheses balance after edits:
  - `emacs -Q --batch --eval '(progn (with-temp-buffer (insert-file-contents "init/shared-init.el") (check-parens)))'`

### One-off batch harnesses

When iterating on a small part of the config (a single function, hook, or
integration), prefer writing a tiny one-off `.el` file under `tmp/` and running
it via `emacs -Q --batch`. This keeps the feedback loop fast without requiring a
full interactive Emacs session.

- Example workflow:
  - Create `tmp/<topic>-test.el` that loads just what you need (either by
    copying the relevant forms or by selectively `load-file`-ing a small file).
  - Run it:
    - `emacs -Q --batch -l tmp/<topic>-test.el`
  - Print output with `princ` and exit non-zero with `(kill-emacs 1)` on
    failure.

## Gotchas

### JSON serialization

When working with JSON serialization issues (especially with LSP
configurations):

- **Test individual components**: Isolate problematic parts by testing JSON
  serialization of individual plists and values to identify the exact issue.

- **Use plists for JSON objects**: `json-serialize` can't handle lists
  containing other lists, but can handle plists:

  ```elisp
  ;; This fails:
  :args '("my-key-1" ("my-key-2" ("--format=unix" "--quiet" "%file")))

  ;; This works:
  :args '(:my-key-1 (:my-key-2 ["--format=unix" "--quiet" "%file"]))
  ```

- **Use vectors for JSON arrays**: `json-serialize` can't handle lists
  containing strings, but can handle vectors:

  ```elisp
  ;; This fails:
  :args '("--format=unix" "--quiet" "%file")

  ;; This works:
  :args ["--format=unix" "--quiet" "%file"]
  ```

- **Keyword vs string handling**: `json-serialize` expects keyword keys in
  plists, but values must be strings. When converting between formats, ensure
  proper type conversion:

  ```elisp
  ;; Convert keywords to strings for values:
  (substring (symbol-name key) 1)  ; Remove leading :
  ```

- **Incremental testing**: Build up the JSON structure piece by piece to isolate
  where serialization fails.

### Deprecated macros

- **`when-let` and `if-let`**: These are deprecated in favor of `when-let*` and
  `if-let*`. Always use the starred versions.
