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

Run unit tests with:

```sh
bun run test
```

This executes the default ERT suite found in `test/test.el`.

Run live smoke tests with:

```sh
bun run test:live
```

This runs live `rass` integration tests in parallel (6 concurrent Emacs
processes by default, configurable via `LIVE_TEST_JOBS` env var). It is slower
than `bun run test`, so do not run it by default after every edit.

Run both unit and live tests together with:

```sh
bun run test:all
```

For debugging a single live test failure, use the sequential runner:

```sh
bun run test:live:sequential
```

Good times to run `bun run test:live`:

- After changing LSP startup or server-contact generation in
  `eglot-python-preset.el`
- After changing generated `rass` preset behavior, tool argv handling, or local
  executable resolution
- After changing PEP-723 environment detection, `uv sync --script` flows, or
  workspace-configuration merging for `ty` or `basedpyright`
- After changing the live harness itself under `test/`
- Before handing off a change that affects real `rass` integration behavior

Usually `bun run test` plus `bun run check` is enough for docs-only changes,
README edits, or purely static test refactors.

### Packaging checks

Run the MELPA-oriented checks with:

```sh
bun run check:melpa
```

This runs:

- `bun run check`
- `bun run check:doc`
- `bun run check:package-lint`
- `bun run check:melpa:install`

There is also a variant that invokes the same four checks plus the full test
suite via bun sub-commands:

```sh
bun run check:melpa:all
```

If you only need one part of that checklist, you can run the sub-commands
directly.

The local source of truth for the MELPA recipe is `eglot-python-preset.recipe`
at the repository root. The local install check and Melpazoid workflow both read
from that file.

`bun run check:melpa:install` performs a local `package-build` build/install
using a temporary snapshot repository outside the working tree. Keep that temp
root outside this repo because `package-build` may run destructive cleanup in
its own clone/build area.

### Checking for byte-compile warnings

Run the byte-compile check with:

```sh
bun run check
```

This byte-compiles `eglot-python-preset.el` and fails if there are any warnings.
Fix all warnings before committing.

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

## Releasing

### Pre-release steps

1. Check for uncommitted changes:

   ```sh
   git status
   ```

   If there are uncommitted changes, offer to commit them before proceeding.

2. Fetch latest tags to ensure we have the complete history:

   ```sh
   git fetch --tags
   ```

3. Update the version in `package.json` and `eglot-python-preset.el` (the
   `Version:` header), then commit the version bump separately from other
   changes with message `chore: bump version to <version>`.

4. Ask the user what tag name they want. Provide examples based on the current
   version:
   - If current version is `0.2.0`:
     - Minor update (new features): `0.3.0`
     - Bugfix update (patches): `0.2.1`

### Creating the release

When the user provides a version (or indicates major/minor/bugfix):

1. Create and push the tag:

   ```sh
   git tag v<version>
   git push origin v<version>
   ```

2. Examine each commit since the last tag to understand the full context:

   ```sh
   git log <previous-tag>..HEAD --oneline
   ```

   For each commit, run `git show <commit>` to see the full commit message and
   diff. Commit messages may be terse or only show the first line in `--oneline`
   output, so examining the full commit is essential for accurate release notes.

3. Create a draft GitHub release:

   ```sh
   gh release create v<version> --draft --title "v<version>" --generate-notes
   ```

4. Enhance the release notes with more context:
   - Use insights from examining each commit in step 2
   - Group related changes under descriptive headings (e.g., "### Refactored X",
     "### Fixed Y")
   - Use bullet lists within each section to describe the changes
   - Include a brief summary of what changed and why it matters
   - Keep the "Full Changelog" link at the bottom
   - Update the release with `gh release edit v<version> --notes "..."`

   Ordering guidelines:
   - Put user-visible changes first (new features, bug fixes, breaking changes)
   - Put under-the-hood changes later (refactoring, internal improvements, docs)
   - Within each section, order by user impact (most impactful first)

5. Tell the user to review the draft release and provide a link:

   ```
   https://github.com/mwolson/eglot-python-preset/releases
   ```
