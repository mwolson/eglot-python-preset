# Plan: Per-project Configuration

## What was done

1. Added `safe-local-variable` predicates for three defcustoms:
   - `eglot-python-preset-lsp-server` -- safe when `ty`, `basedpyright`, or
     `rass`
   - `eglot-python-preset-rass-tools` -- safe when a list of known symbols only
     (vectors excluded since they can execute arbitrary programs)
   - `eglot-python-preset-python-project-markers` -- safe when a list of strings

2. Added tests for all three predicates.

3. Added README section documenting both `.dir-locals.el` and
   `dir-locals-set-directory-class` approaches.

## Audit of `~/emacs-shared/init/shared-init.el`

### `my-apheleia-skip-bun` (shared-init.el:1477-1479)

Inhibits formatting for `bun.lock` files by matching on filename. This is
file-specific, not directory-specific, so dir-locals is not the right fit. Keep
as-is.

### Formatter detection functions (shared-init.el:428-458)

`my-detect-js-formatter`, `my-detect-markdown-formatter`,
`my-detect-yaml-formatter` auto-detect formatters by looking for config files at
runtime. This is more flexible than static dir-locals and should stay as-is. The
dir-locals approach is useful as a fallback when auto-detection gives the wrong
answer for a specific project.

### `eglot-python-preset-lsp-server` global default (shared-init.el:1711)

```elisp
(setopt eglot-python-preset-lsp-server 'ty)
```

Now that `eglot-python-preset-lsp-server` has a `safe-local-variable` property,
projects that need a different server can use either `.dir-locals.el` or
`dir-locals-set-directory-class`. No changes needed here.

### Unrelated issue: `my-js--project-find` project type (shared-init.el:344-349)

Not part of this task, but worth noting: `my-js--project-find` returns
`(cons 'python-project root)` while line 1484-1487 defines a `project-root`
method for `(head js-project)`. The method for `js-project` is unreachable. This
works today because `python-project` has its own `project-root` method (from
eglot-python-preset), but it is fragile if eglot-python-preset is not loaded
when JS files are opened.
