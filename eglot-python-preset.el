;;; eglot-python-preset.el --- Eglot preset for Python -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Michael Olson <mwolson@gnu.org>

;; Version: 0.2.0
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

;; Quick start:
;;
;;   (require 'eglot-python-preset)
;;   (setopt eglot-python-preset-lsp-server 'ty)
;;   ;; or
;;   ;; (setopt eglot-python-preset-lsp-server 'basedpyright)
;;   (eglot-python-preset-setup)
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

(declare-function eglot--managed-buffers "eglot")
(declare-function eglot-current-server "eglot")
(declare-function eglot-ensure "eglot")
(declare-function eglot-shutdown "eglot")

(defvar eglot-server-programs)
(defvar eglot-workspace-configuration)

(defgroup eglot-python-preset nil
  "Python preset for Eglot."
  :group 'eglot
  :prefix "eglot-python-preset-")

;;;###autoload
(defcustom eglot-python-preset-lsp-server 'ty
  "LSP server to use for Python files."
  :type '(choice (const :tag "ty" ty)
                 (const :tag "basedpyright" basedpyright))
  :group 'eglot-python-preset)

;;;###autoload
(defcustom eglot-python-preset-python-project-markers
  '("pyproject.toml" "requirements.txt")
  "Files that indicate a Python project root."
  :type '(repeat string)
  :group 'eglot-python-preset)

(defun eglot-python-preset-has-metadata-p ()
  "Return non-nil if current buffer contains PEP-723 script metadata."
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
    (when-let* ((file (buffer-file-name))
                ((eglot-python-preset-has-metadata-p))
                (script-dir (file-name-directory file))
                (python-path (eglot-python-preset-get-python-path file)))
      (let ((env-dir (or (eglot-python-preset--python-env-dir python-path)
                         python-path)))
        `(:configuration
          (:environment
           (:python ,env-dir
            :root [,script-dir])))))))

(defun eglot-python-preset--server-contact (_interactive)
  "Return the server contact spec for Python LSP.

Includes initializationOptions for ty with PEP-723 scripts."
  (let ((command (pcase eglot-python-preset-lsp-server
                   ('ty '("ty" "server"))
                   ('basedpyright '("basedpyright-langserver" "--stdio"))))
        (init-options (eglot-python-preset--init-options)))
    (if init-options
        `(,@command :initializationOptions ,init-options)
      command)))

(defun eglot-python-preset--python-project-root-p (dir)
  "Return non-nil if DIR contains a Python project marker file."
  (seq-some (lambda (file)
              (file-exists-p (expand-file-name file dir)))
            eglot-python-preset-python-project-markers))

(defun eglot-python-preset--project-find (dir)
  "Project detection for Python files.

For PEP-723 scripts, returns (python-script . SCRIPT-PATH) so each script
gets its own eglot server instance.
Otherwise, returns (python-project . ROOT) if DIR is inside a Python project."
  (cond
   ((and (derived-mode-p 'python-base-mode)
         (eglot-python-preset-has-metadata-p))
    (cons 'python-script (buffer-file-name)))
   ((when-let* ((root (locate-dominating-file
                       dir #'eglot-python-preset--python-project-root-p)))
      (cons 'python-project root)))))

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

;;;###autoload
(defun eglot-python-preset-setup ()
  "Set up Eglot to support Python modes, including PEP-723 support.

Adds hooks for project detection and Eglot configuration.
Configures `eglot-server-programs' based on `eglot-python-preset-lsp-server'.
Call this after loading Eglot."
  (interactive)
  (add-to-list 'eglot-server-programs
               '((python-ts-mode python-mode) .
                 eglot-python-preset--server-contact))
  (add-hook 'project-find-functions #'eglot-python-preset--project-find)
  (advice-add 'eglot--workspace-configuration-plist :around
              #'eglot-python-preset--workspace-configuration-plist-a)
  (add-hook 'python-base-mode-hook #'eglot-ensure t))

(provide 'eglot-python-preset)
;;; eglot-python-preset.el ends here
