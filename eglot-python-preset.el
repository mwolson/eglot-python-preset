;;; eglot-python-preset.el --- Eglot preset for Python -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Michael Olson <mwolson@gnu.org>

;; Version: 0.4.0
;; Author: Michael Olson <mwolson@gnu.org>
;; Maintainer: Michael Olson <mwolson@gnu.org>
;; URL: https://github.com/mwolson/eglot-python-preset
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Keywords: python, convenience, languages, tools
;; Package-Requires: ((emacs "30.2") (eglot "1.17"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a preset for Eglot to work with Python files,
;; including support for PEP-723 script metadata and project detection.
;; It configures the LSP server (ty or basedpyright) and handles environment
;; synchronization for uv-managed scripts.

;; Prerequisites:
;;
;; - Install uv
;; - Install ty (>= v0.0.8) or basedpyright as a Python language server
;; - Download this file and add it to the load path

;; Quick start (package-vc):
;;
;;   (use-package eglot-python-preset
;;     :vc (:url "https://github.com/mwolson/eglot-python-preset")
;;     :custom
;;     (eglot-python-preset-lsp-server 'ty)) ; or 'basedpyright or 'rass
;;
;; After that, opening Python files will automatically start the LSP server using
;; Eglot and handle PEP-723 magic tags within files.

;; Workspace configuration (basedpyright):
;;
;; To customize basedpyright settings, set `eglot-workspace-configuration'
;; before calling `eglot-python-preset-setup'.  Your settings will be merged
;; with PEP-723 script configurations (which add :python :pythonPath).
;;
;; Example:
;;
;;   (setopt eglot-workspace-configuration
;;           '(:basedpyright.analysis
;;             (:autoImportCompletions :json-false
;;              :typeCheckingMode "basic")))
;;   (eglot-python-preset-setup)

;;; Code:

(require 'cl-lib)
(require 'json)

(declare-function eglot-client-capabilities "eglot")
(declare-function eglot--major-modes "eglot")
(declare-function eglot--managed-buffers "eglot")
(declare-function eglot-current-server "eglot")
(declare-function eglot-ensure "eglot")
(declare-function eglot-handle-notification "eglot")
(declare-function eglot-shutdown "eglot")

(defvar eglot-server-programs)
(defvar eglot-workspace-configuration)

(defgroup eglot-python-preset nil
  "Python preset for Eglot."
  :group 'eglot
  :prefix "eglot-python-preset-")

;;;###autoload
(defcustom eglot-python-preset-auto-setup t
  "Whether to automatically set up Eglot for Python when eglot loads.
When non-nil, `eglot-python-preset-setup' is called automatically
via an autoloaded `with-eval-after-load' form.  Set to nil before the
package is loaded to suppress automatic setup and call
`eglot-python-preset-setup' manually instead."
  :type 'boolean
  :group 'eglot-python-preset)

;;;###autoload
(defcustom eglot-python-preset-lsp-server 'ty
  "LSP server to use for Python files."
  :type '(choice (const :tag "ty" ty)
                 (const :tag "basedpyright" basedpyright)
                 (const :tag "rass" rass))
  :group 'eglot-python-preset)

;;;###autoload
(defcustom eglot-python-preset-rass-program "rass"
  "Program used when `eglot-python-preset-lsp-server' is `rass'."
  :type 'string
  :group 'eglot-python-preset)

;;;###autoload
(defcustom eglot-python-preset-rass-command nil
  "Exact command vector to run when `eglot-python-preset-lsp-server' is `rass'.

When non-nil, this command is used verbatim and no generated preset is written.
The value must include the `rass` executable and any preset or arguments that
should be passed to it."
  :type '(choice
          (const :tag "Use generated preset" nil)
          (restricted-sexp
           :tag "Exact command vector"
           :value ["rass" "python"]
           :match-alternatives (eglot-python-preset--rass-command-vector-p)))
  :group 'eglot-python-preset)

(defcustom eglot-python-preset-rass-max-contextual-presets 50
  "Maximum number of contextual generated `rass` presets to keep.

When more contextual presets are present in the generated preset directory,
older ones are deleted.  Shared presets are not affected."
  :type '(choice (const :tag "Disable cleanup" nil)
                 (integer :tag "Maximum contextual presets"))
  :group 'eglot-python-preset)

(defun eglot-python-preset--rass-command-vector-p (value)
  "Return non-nil if VALUE is a vector-form literal command."
  (and (vectorp value)
       (> (length value) 0)
       (seq-every-p #'stringp value)))

;;;###autoload
(defcustom eglot-python-preset-rass-tools '(ty ruff)
  "Tools included in the generated `rass` preset.

Each entry may be a supported symbol like `ty', `ruff', or `basedpyright',
or a literal command vector of strings.  Literal commands are passed through
as-is, except that known executables still get local `.venv` resolution and
any supported special handling."
  :type '(repeat
          (choice
           (const :tag "basedpyright" basedpyright)
           (const :tag "ruff" ruff)
           (const :tag "ty" ty)
           (restricted-sexp
            :tag "Command vector"
            :value ["command"]
            :match-alternatives (eglot-python-preset--rass-command-vector-p))))
  :group 'eglot-python-preset)

;;;###autoload
(defcustom eglot-python-preset-python-modes
  '(python-mode python-ts-mode)
  "Major modes for Python files."
  :type '(repeat symbol)
  :group 'eglot-python-preset)

;;;###autoload
(defcustom eglot-python-preset-python-project-markers
  '("pyproject.toml" "requirements.txt")
  "Files that indicate a Python project root."
  :type '(repeat string)
  :group 'eglot-python-preset)

(defun eglot-python-preset--lsp-server-safe-p (value)
  "Return non-nil if VALUE is a safe `eglot-python-preset-lsp-server' value."
  (memq value '(ty basedpyright rass)))

(put 'eglot-python-preset-lsp-server 'safe-local-variable
     #'eglot-python-preset--lsp-server-safe-p)

(defun eglot-python-preset--rass-tools-safe-p (value)
  "Return non-nil if VALUE is a safe `eglot-python-preset-rass-tools' value.
Only lists of known symbols are considered safe.  Literal command vectors
are excluded because they could execute arbitrary programs."
  (and (listp value)
       (seq-every-p (lambda (item) (memq item '(basedpyright ruff ty))) value)))

(put 'eglot-python-preset-rass-tools 'safe-local-variable
     #'eglot-python-preset--rass-tools-safe-p)

(defun eglot-python-preset--modes-safe-p (value)
  "Return non-nil if VALUE is a safe `eglot-python-preset-python-modes'."
  (and (listp value)
       (seq-every-p #'symbolp value)))

(put 'eglot-python-preset-python-modes 'safe-local-variable
     #'eglot-python-preset--modes-safe-p)

(defun eglot-python-preset--all-managed-modes ()
  "Return a list of all major modes managed by this preset."
  eglot-python-preset-python-modes)

(defun eglot-python-preset--project-markers-safe-p (value)
  "Return non-nil if VALUE is a safe `eglot-python-preset-python-project-markers'."
  (and (listp value)
       (seq-every-p #'stringp value)))

(put 'eglot-python-preset-python-project-markers 'safe-local-variable
     #'eglot-python-preset--project-markers-safe-p)

(defun eglot-python-preset-has-metadata-p ()
  "Return non-nil if current buffer has PEP-723 script metadata."
  (let ((case-fold-search nil))
    (when (buffer-file-name)
      (save-match-data
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (when (re-search-forward "^# /// script$" nil t)
              (re-search-forward "^# ///$" nil t))))))))

(defun eglot-python-preset--uv-env-dir ()
  "Return the uv script environments directory.

Uses `uv cache dir` to get the cache location, then appends environments-v2/."
  (let ((cache-dir (string-trim (shell-command-to-string "uv cache dir"))))
    (unless (string-empty-p cache-dir)
      (file-name-as-directory
       (expand-file-name "environments-v2" cache-dir)))))

(defvar eglot-python-preset--warned-scripts (make-hash-table :test 'equal)
  "Hash table tracking scripts we've warned about unsynced environments.")

(defun eglot-python-preset-get-python-path (script-path)
  "Get Python interpreter path for SCRIPT-PATH using uv.

Runs `uv python find --script SCRIPT-PATH'.
Displays a warning if the environment needs to be synced.
Returns the Python path, or nil if uv is not available."
  (if (not (executable-find "uv"))
      (progn
        (message "[eglot-python-preset] uv not found")
        nil)
    (let* ((script-path (expand-file-name script-path))
           (env-dir (eglot-python-preset--uv-env-dir))
           (default-directory (file-name-directory script-path))
           (output (shell-command-to-string
                    (format "uv python find --script %s"
                            (shell-quote-argument script-path))))
           (python-path (string-trim output)))
      (cond
       ((string-empty-p python-path)
        (message "[eglot-python-preset] uv couldn't find Python for %s"
                 script-path)
        nil)
       ((and env-dir (string-prefix-p env-dir python-path))
        python-path)
       (t
        (unless (gethash script-path eglot-python-preset--warned-scripts)
          (puthash script-path t eglot-python-preset--warned-scripts)
          (display-warning
           'eglot-python-preset
           "Environment not synced. Run M-x eglot-python-preset-sync-environment"
           :warning))
        python-path)))))

(defun eglot-python-preset--python-env-dir (python-path)
  "Return the environment directory for PYTHON-PATH.

Given a path like /path/to/env/bin/python3, return /path/to/env/."
  (when python-path
    (let ((bin-dir (file-name-directory python-path)))
      (when (string-match-p "/bin/?$" bin-dir)
        (file-name-directory (directory-file-name bin-dir))))))

(defun eglot-python-preset--project-root ()
  "Return the current buffer's project root.
For PEP-723 scripts without project markers, returns the script's
directory so that local .venv resolution still works."
  (when-let* ((file (buffer-file-name)))
    (or (when-let* ((root (locate-dominating-file
                           (file-name-directory file)
                           #'eglot-python-preset--python-project-root-p)))
          (file-name-as-directory root))
        (when (and (derived-mode-p 'python-base-mode)
                   (eglot-python-preset-has-metadata-p))
          (file-name-directory file)))))

(defun eglot-python-preset--venv-bin-dirs ()
  "Return candidate local virtualenv executable directories."
  (when-let* ((root (eglot-python-preset--project-root))
              (venv-dir (expand-file-name ".venv/" root)))
    (delq nil
          (mapcar (lambda (path)
                    (when (file-directory-p path)
                      path))
                  (list (expand-file-name "bin" venv-dir)
                        (expand-file-name "Scripts" venv-dir))))))

(defun eglot-python-preset--resolve-executable (name)
  "Resolve executable NAME, preferring the current project's local `.venv'."
  (or (cl-loop for dir in (eglot-python-preset--venv-bin-dirs)
               thereis (let ((exec-path (cons dir exec-path)))
                         (executable-find name)))
      (executable-find name)
      name))

(defun eglot-python-preset--pep-723-context ()
  "Return PEP-723 environment context for the current buffer, or nil."
  (when-let* ((file (buffer-file-name))
              ((eglot-python-preset-has-metadata-p))
              (script-dir (file-name-directory file))
              (python-path (eglot-python-preset-get-python-path file)))
    (list :python-path python-path
          :script-dir script-dir
          :env-dir (or (eglot-python-preset--python-env-dir python-path)
                       python-path))))

(defun eglot-python-preset--ty-configuration (&optional context)
  "Return ty configuration for CONTEXT."
  (when-let* ((context (or context (eglot-python-preset--pep-723-context)))
              (env-dir (plist-get context :env-dir))
              (script-dir (plist-get context :script-dir)))
    `(:configuration
      (:environment
       (:python ,env-dir
        :root [,script-dir])))))

(defun eglot-python-preset--basedpyright-python-configuration (&optional context)
  "Return basedpyright Python configuration for CONTEXT."
  (when-let* ((context (or context (eglot-python-preset--pep-723-context)))
              (python-path (plist-get context :python-path)))
    `(:pythonPath ,python-path)))

(defun eglot-python-preset--tool-kind-from-name (name)
  "Return the known tool kind for executable NAME, or nil."
  (when name
    (let ((base (downcase (file-name-sans-extension
                           (file-name-nondirectory name)))))
      (cond
       ((member base '("basedpyright" "basedpyright-langserver"))
        'basedpyright)
       ((string= base "ruff")
        'ruff)
       ((string= base "ty")
        'ty)))))

(defun eglot-python-preset--rass-tool-command (tool)
  "Return the server command for `rass` TOOL."
  (cond
   ((eq tool 'basedpyright)
    (list (eglot-python-preset--resolve-executable "basedpyright-langserver")
          "--stdio"))
   ((eq tool 'ruff)
    (list (eglot-python-preset--resolve-executable "ruff")
          "server"))
   ((eq tool 'ty)
    (list (eglot-python-preset--resolve-executable "ty")
          "server"))
   ((vectorp tool)
    (let* ((command (append tool nil))
           (kind (eglot-python-preset--tool-kind-from-name (car command))))
      (when kind
        (setcar command (eglot-python-preset--resolve-executable (car command))))
      command))
   (t
    (user-error "Unsupported `rass` tool entry: %S" tool))))

(defun eglot-python-preset--rass-tool-kind (command)
  "Return the supported tool kind for COMMAND, or nil."
  (eglot-python-preset--tool-kind-from-name (car command)))

(defun eglot-python-preset--rass-json-string (object)
  "Serialize OBJECT to JSON."
  (decode-coding-string (json-serialize object) 'utf-8))

(defun eglot-python-preset--rass-generated-dir ()
  "Return the directory used for generated `rass` presets."
  (expand-file-name "eglot-python-preset/" user-emacs-directory))

(defun eglot-python-preset--library-dir ()
  "Return directory containing the installed library."
  (file-name-directory
   (or load-file-name
       byte-compile-current-file
       (locate-library "eglot-python-preset")
       default-directory)))

(defun eglot-python-preset--path-in-directory-p (path dir)
  "Return non-nil if PATH is inside DIR."
  (when (and path dir)
    (let ((path (file-truename path))
          (dir (file-name-as-directory (file-truename dir))))
      (string-prefix-p dir path))))

(defvar eglot-python-preset--template-cache (make-hash-table :test 'equal)
  "Cache for package template contents.")

(defun eglot-python-preset--template-string (name)
  "Return template NAME from the installed package."
  (let* ((path (expand-file-name
                name (expand-file-name
                      "templates" (eglot-python-preset--library-dir))))
         (attrs (file-attributes path))
         (mtime (file-attribute-modification-time attrs))
         (cached (gethash name eglot-python-preset--template-cache)))
    (if (and cached
             (equal (plist-get cached :path) path)
             (equal (plist-get cached :mtime) mtime))
        (plist-get cached :content)
      (let ((content
             (with-temp-buffer
               (insert-file-contents path)
               (buffer-string))))
        (puthash name
                 (list :content content :mtime mtime :path path)
                 eglot-python-preset--template-cache)
        content))))

(defun eglot-python-preset--render-template (name replacements)
  "Render template NAME using REPLACEMENTS.

REPLACEMENTS is an alist mapping literal placeholder strings to values."
  (let ((template (eglot-python-preset--template-string name)))
    (dolist (replacement replacements template)
      (setq template
            (replace-regexp-in-string
             (regexp-quote (car replacement))
             (cdr replacement)
             template
             t t)))))

(defun eglot-python-preset--rass-tool-label (tool)
  "Return a stable filename label for `rass` TOOL."
  (let* ((text (cond
                ((symbolp tool)
                 (symbol-name tool))
                ((vectorp tool)
                 (let* ((command (append tool nil))
                        (program (file-name-sans-extension
                                  (file-name-nondirectory (car command))))
                        (base (if (string-empty-p program)
                                  "tool"
                                program)))
                   (if (= (length command) 1)
                       base
                     (format "%s-argv-%s"
                             base
                             (substring (secure-hash 'sha256 (prin1-to-string tool))
                                        0 12)))))
                (t
                 (user-error "Unsupported `rass` tool entry: %S" tool)))))
    (string-trim
     (replace-regexp-in-string
      "-+"
      "-"
      (replace-regexp-in-string "[^[:alnum:]]+" "-" (downcase text)))
     "-"
     "-")))

(defun eglot-python-preset--rass-shared-preset-path (tools)
  "Return the stable shared preset path for TOOLS."
  (let* ((labels (mapcar #'eglot-python-preset--rass-tool-label tools))
         (slug (string-join labels "-"))
         (hash (substring (secure-hash 'sha256 (prin1-to-string tools)) 0 12)))
    (expand-file-name
     (format "rass-preset-shared-%s-%s.py" slug hash)
     (eglot-python-preset--rass-generated-dir))))

(defun eglot-python-preset--rass-contextual-preset-path (hash-input)
  "Return the contextual preset path for HASH-INPUT."
  (expand-file-name
   (format "rass-preset-contextual-%s.py"
           (secure-hash 'sha256 hash-input))
   (eglot-python-preset--rass-generated-dir)))

(defun eglot-python-preset--rass-tool-spec (tool)
  "Return metadata for `rass` TOOL."
  (let* ((command (eglot-python-preset--rass-tool-command tool))
         (kind (eglot-python-preset--rass-tool-kind command))
         (program (car command)))
    (list :tool tool
          :command command
          :kind kind
          :local-venv-sensitive
          (and kind
               (stringp program)
               (seq-some (lambda (dir)
                           (eglot-python-preset--path-in-directory-p program dir))
                         (eglot-python-preset--venv-bin-dirs))))))

(defun eglot-python-preset--write-file-if-changed (path content)
  "Write CONTENT to PATH only when it differs from the existing file."
  (make-directory (file-name-directory path) t)
  (unless (and (file-exists-p path)
               (with-temp-buffer
                 (insert-file-contents path)
                 (string= (buffer-string) content)))
    (with-temp-file path
      (insert content))))

(defun eglot-python-preset--cleanup-rass-contextual-presets (&optional preserve-path)
  "Delete older contextual generated `rass` presets, preserving PRESERVE-PATH."
  (when-let* (((integerp eglot-python-preset-rass-max-contextual-presets))
              ((> eglot-python-preset-rass-max-contextual-presets 0))
              (dir (eglot-python-preset--rass-generated-dir))
              ((file-directory-p dir)))
    (let ((files (directory-files dir t "^rass-preset-contextual-.*\\.py\\'")))
      (setq files
            (sort files
                  (lambda (a b)
                    (time-less-p
                     (file-attribute-modification-time (file-attributes b))
                     (file-attribute-modification-time (file-attributes a))))))
      (when preserve-path
        (setq files
              (cons preserve-path
                    (delete preserve-path files))))
      (let ((overflow (nthcdr eglot-python-preset-rass-max-contextual-presets files)))
        (dolist (file overflow)
          (when (file-exists-p file)
            (delete-file file)))))))

(defun eglot-python-preset--write-rass-preset (path commands ty-config
                                                    basedpyright-config)
  "Write a generated `rass` preset to PATH.

COMMANDS is the list of server commands.
TY-CONFIG and BASEDPYRIGHT-CONFIG contain any extra per-server
configuration to merge via `workspace/configuration'."
  (eglot-python-preset--write-file-if-changed
   path
   (eglot-python-preset--render-template
    "rass-preset.tpl.py"
    `(("__SERVERS__"
       . ,(eglot-python-preset--rass-json-string
           (vconcat (mapcar #'vconcat commands))))
      ("__TY_CONFIGURATION__"
       . ,(if ty-config
              (eglot-python-preset--rass-json-string ty-config)
            "None"))
      ("__BASEDPYRIGHT_CONFIGURATION__"
       . ,(if basedpyright-config
              (eglot-python-preset--rass-json-string basedpyright-config)
            "None"))))))

(defun eglot-python-preset--rass-preset-path ()
  "Return the generated `rass` preset path for the current buffer context."
  (let* ((tool-specs (mapcar #'eglot-python-preset--rass-tool-spec
                             eglot-python-preset-rass-tools))
         (commands (mapcar (lambda (tool-spec)
                             (plist-get tool-spec :command))
                           tool-specs))
         (context (eglot-python-preset--pep-723-context))
         (ty-config (when (and context
                               (seq-some (lambda (command)
                                           (eq (eglot-python-preset--rass-tool-kind command)
                                               'ty))
                                         commands))
                      (eglot-python-preset--ty-configuration context)))
         (basedpyright-config
          (when (and context
                     (seq-some (lambda (command)
                                 (eq (eglot-python-preset--rass-tool-kind command)
                                     'basedpyright))
                               commands))
            (eglot-python-preset--basedpyright-python-configuration context)))
         (contextual-p (or ty-config
                           basedpyright-config
                           (seq-some (lambda (tool-spec)
                                       (plist-get tool-spec :local-venv-sensitive))
                                     tool-specs)))
         (path (if contextual-p
                   (eglot-python-preset--rass-contextual-preset-path
                    (mapconcat #'identity
                               (delq nil
                                     (list
                                      (eglot-python-preset--rass-json-string
                                       (vconcat (mapcar #'vconcat commands)))
                                      (when ty-config
                                        (eglot-python-preset--rass-json-string
                                         ty-config))
                                      (when basedpyright-config
                                        (eglot-python-preset--rass-json-string
                                         basedpyright-config))))
                               "\0"))
                 (eglot-python-preset--rass-shared-preset-path
                  eglot-python-preset-rass-tools))))
    (eglot-python-preset--write-rass-preset
     path commands ty-config basedpyright-config)
    (when contextual-p
      (eglot-python-preset--cleanup-rass-contextual-presets path))
    path))

(defun eglot-python-preset--merge-plists (base override)
  "Recursively merge OVERRIDE plist into BASE plist.

For keys present in both, OVERRIDE values take precedence.
If both values are plists, merge them recursively."
  (let ((result (copy-sequence base)))
    (cl-loop for (key val) on override by #'cddr
             do (let ((base-val (plist-get result key)))
                  (setq result
                        (plist-put result key
                                   (if (and (listp base-val)
                                            (listp val)
                                            (keywordp (car-safe base-val))
                                            (keywordp (car-safe val)))
                                       (eglot-python-preset--merge-plists
                                        base-val val)
                                     val)))))
    result))

(defun eglot-python-preset--workspace-configuration-plist-a (orig-fn server &optional path)
  "Advice to merge PEP-723 Python path into workspace configuration.

Calls ORIG-FN with SERVER and PATH arguments, then merges :python :pythonPath
for PEP-723 scripts when using basedpyright.

Gets the first managed buffer from SERVER to check for PEP-723 metadata,
since PATH is typically a directory (project root) or nil."
  (let ((base-config (funcall orig-fn server path)))
    (if-let* (((eq eglot-python-preset-lsp-server 'basedpyright))
              (buf (car (eglot--managed-buffers server)))
              (file-path (buffer-file-name buf))
              ((with-current-buffer buf (eglot-python-preset-has-metadata-p)))
              (python-path (eglot-python-preset-get-python-path file-path)))
        (eglot-python-preset--merge-plists
         base-config
         `(:python (:pythonPath ,python-path)))
      base-config)))

(defun eglot-python-preset--init-options ()
  "Return initializationOptions for ty LSP server.

For PEP-723 scripts, includes environment configuration.
Only used for ty; basedpyright uses workspace configuration instead."
  (when (eq eglot-python-preset-lsp-server 'ty)
    (eglot-python-preset--ty-configuration)))

(defun eglot-python-preset--server-contact (_interactive)
  "Return the server contact spec for Python LSP.

Includes initializationOptions for ty with PEP-723 scripts."
  (let ((command (pcase eglot-python-preset-lsp-server
                   ('basedpyright
                    (list (eglot-python-preset--resolve-executable
                           "basedpyright-langserver")
                          "--stdio"))
                   ('rass
                    (if eglot-python-preset-rass-command
                        (append eglot-python-preset-rass-command nil)
                      (list (eglot-python-preset--resolve-executable
                             eglot-python-preset-rass-program)
                            (eglot-python-preset--rass-preset-path))))
                   ('ty (list (eglot-python-preset--resolve-executable "ty")
                              "server"))))
        (init-options (eglot-python-preset--init-options)))
    (if init-options
        `(,@command :initializationOptions ,init-options)
      command)))

(defun eglot-python-preset--in-indirect-md-buffer-p ()
  "Return non-nil if buffer is an indirect buffer from a markdown buffer."
  (when-let* ((buf (buffer-base-buffer))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (derived-mode-p 'markdown-mode))))

(defun eglot-python-preset--python-project-root-p (dir)
  "Return non-nil if DIR has a Python project marker file."
  (seq-some (lambda (file)
              (file-exists-p (expand-file-name file dir)))
            eglot-python-preset-python-project-markers))

(defvar eglot-lsp-context)

(defun eglot-python-preset--project-find (dir)
  "Project detection for Python files.

For PEP-723 scripts, returns (python-script . SCRIPT-PATH) so each script
gets its own eglot server instance.
Otherwise, returns (python-project . ROOT) if DIR is inside a Python project.
Only activates when `eglot-lsp-context' is non-nil and the current
buffer is in a Python major mode, so that non-Python buffers
\(e.g. TypeScript files in a polyglot project) fall through to other
project backends."
  (when (and (bound-and-true-p eglot-lsp-context)
             (not (eglot-python-preset--in-indirect-md-buffer-p))
             (apply #'derived-mode-p
                    (eglot-python-preset--all-managed-modes)))
    (cond
     ((eglot-python-preset-has-metadata-p)
      (cons 'python-script (buffer-file-name)))
     ((when-let* ((root (locate-dominating-file
                          dir #'eglot-python-preset--python-project-root-p)))
        (cons 'python-project root))))))

(cl-defmethod project-root ((project (head python-script)))
  "Return directory containing the script for PROJECT."
  (file-name-directory (cdr project)))

(cl-defmethod project-root ((project (head python-project)))
  "Return root directory of PROJECT."
  (cdr project))

;;;###autoload
(defun eglot-python-preset-sync-environment ()
  "Sync the current PEP-723 script's environment, then restart Eglot.

Runs `uv sync --script' on the current file.
Uses shutdown + `eglot-ensure' instead of reconnect so that
initializationOptions are recomputed (needed for ty)."
  (interactive)
  (let ((script-path (buffer-file-name)))
    (unless script-path
      (user-error "No file associated with buffer"))
    (unless (executable-find "uv")
      (user-error "Installation for uv not found"))
    (unless (eglot-python-preset-has-metadata-p)
      (user-error "Buffer does not contain PEP-723 metadata"))
    (let* ((default-directory (file-name-directory script-path))
           (status (call-process "uv" nil nil nil "sync" "--script"
                                 script-path)))
      (if (zerop status)
          (progn
            (remhash script-path eglot-python-preset--warned-scripts)
            (when-let* ((win (get-buffer-window "*Warnings*")))
              (delete-window win))
            (message "Environment synced successfully, restarting eglot")
            (when-let* ((server (eglot-current-server)))
              (eglot-shutdown server))
            (eglot-ensure))
        (message "Failed to sync environment (exit code: %s)" status)))))

;;;###autoload
(defun eglot-python-preset-run-script ()
  "Run the current file as a PEP-723 script using `uv run'."
  (interactive)
  (let ((script-path (buffer-file-name)))
    (unless script-path
      (user-error "No file associated with buffer"))
    (unless (eglot-python-preset-has-metadata-p)
      (user-error "Buffer does not contain PEP-723 metadata"))
    (let ((default-directory (file-name-directory script-path)))
      (compile (format "uv run %s" (shell-quote-argument script-path))))))

;;;###autoload
(defun eglot-python-preset-remove-environment ()
  "Remove the cached uv environment for the current PEP-723 script.

This deletes the environment directory created by uv for this script.
After removal, run `eglot-python-preset-sync-environment' to recreate it."
  (interactive)
  (let ((script-path (buffer-file-name)))
    (unless script-path
      (user-error "No file associated with buffer"))
    (unless (executable-find "uv")
      (user-error "Installation for uv not found"))
    (unless (eglot-python-preset-has-metadata-p)
      (user-error "Buffer does not contain PEP-723 metadata"))
    (let ((python-path (eglot-python-preset-get-python-path script-path)))
      (unless python-path
        (user-error "No environment found for this script"))
      (let ((env-dir (eglot-python-preset--python-env-dir python-path))
            (uv-env-dir (eglot-python-preset--uv-env-dir)))
        (unless env-dir
          (user-error "Could not determine environment directory"))
        (unless (file-directory-p env-dir)
          (user-error "Environment directory does not exist: %s" env-dir))
        (unless (and uv-env-dir (string-prefix-p uv-env-dir env-dir))
          (user-error "No uv-managed PEP-723 environment detected:\n\
Run `eglot-python-preset-sync-environment' to create a managed environment"))
        (when (yes-or-no-p (format "Delete environment %s? " env-dir))
          (when-let* ((server (eglot-current-server)))
            (eglot-shutdown server))
          (delete-directory env-dir t)
          (message "Environment removed: %s" env-dir))))))

(defun eglot-python-preset--our-server-p (server)
  "Return non-nil if SERVER manages modes from this preset."
  (cl-some (lambda (m) (memq m eglot-python-preset-python-modes))
           (eglot--major-modes server)))

(defvar eglot-python-preset--streaming-diag-table (make-hash-table :test #'equal)
  "Per-URI hash of streaming diagnostic tokens.
Each key is a URI string.  Each value is a hash table mapping
token strings to their latest diagnostics vector.")

(defun eglot-python-preset--client-capabilities-a (orig-fn server)
  "Advice to inject streaming diagnostics capability.
Calls ORIG-FN with SERVER and adds :$streamingDiagnostics t to the
:textDocument plist so that rass enables pull-to-push streaming.
Only applies to servers managing modes from this preset."
  (let ((caps (funcall orig-fn server)))
    (when (eglot-python-preset--our-server-p server)
      (clrhash eglot-python-preset--streaming-diag-table)
      (let ((td (plist-get caps :textDocument)))
        (when td
          (plist-put td :$streamingDiagnostics t))))
    caps))

(cl-defmethod eglot-handle-notification
  (server (_method (eql $/streamDiagnostics)) &key uri token diagnostics
          &allow-other-keys)
  "Accumulate per-token diagnostics and publish the merged set.
SERVER receives the merged diagnostics for URI.  Each TOKEN
identifies a sub-server whose DIAGNOSTICS are stored separately,
then all tokens for the URI are combined before forwarding to
the standard publishDiagnostics handler."
  (let ((by-token (or (gethash uri eglot-python-preset--streaming-diag-table)
                      (puthash uri (make-hash-table :test #'equal)
                               eglot-python-preset--streaming-diag-table))))
    (puthash token (or diagnostics []) by-token)
    (let ((merged []))
      (maphash (lambda (_tok diags) (setq merged (vconcat merged diags))) by-token)
      (eglot-handle-notification server 'textDocument/publishDiagnostics
                                 :uri uri :diagnostics merged))))

(defvar eglot-python-preset--setup-done nil
  "Non-nil if `eglot-python-preset-setup' has already run.")

;;;###autoload
(defun eglot-python-preset-setup ()
  "Set up Eglot to support Python modes, including PEP-723 support.

Adds hooks for project detection and Eglot configuration.
Configures `eglot-server-programs' based on `eglot-python-preset-lsp-server'."
  (interactive)
  (require 'eglot)
  (unless eglot-python-preset--setup-done
    (setq eglot-python-preset--setup-done t)
    (add-to-list 'eglot-server-programs
                 `(,eglot-python-preset-python-modes .
                   eglot-python-preset--server-contact))
    (add-hook 'project-find-functions #'eglot-python-preset--project-find)
    (advice-add 'eglot-client-capabilities :around
                #'eglot-python-preset--client-capabilities-a)
    (advice-add 'eglot--workspace-configuration-plist :around
                #'eglot-python-preset--workspace-configuration-plist-a)
    (add-hook 'python-base-mode-hook #'eglot-ensure t)))

;;;###autoload
(progn
  (defvar eglot-python-preset--maybe-setup-in-progress nil)
  (defun eglot-python-preset--maybe-setup ()
    "Set up Eglot for Python if auto-setup is enabled."
    (unless eglot-python-preset--maybe-setup-in-progress
      (let ((eglot-python-preset--maybe-setup-in-progress t))
        (when eglot-python-preset-auto-setup
          (require 'eglot-python-preset nil t)
          (eglot-python-preset-setup)))))
  (unless noninteractive
    (if after-init-time
        (eglot-python-preset--maybe-setup)
      (add-hook 'after-init-hook #'eglot-python-preset--maybe-setup t))))

(provide 'eglot-python-preset)
;;; eglot-python-preset.el ends here
