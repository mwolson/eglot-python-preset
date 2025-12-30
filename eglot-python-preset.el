;;; eglot-python-preset.el --- Eglot preset for Python -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Michael Olson <mwolson@gnu.org>

;; Version: 0.1.0
;; Author: Michael Olson <mwolson@gnu.org>
;; Maintainer: Michael Olson <mwolson@gnu.org>
;; URL: https://github.com/mwolson/eglot-python-preset
;; Keywords: python, convenience, languages, tools
;; Package-Requires: ((emacs "30.2") (eglot "1.17"))

;; This file is NOT part of GNU Emacs.

;; eglot-python-preset is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with eglot-python-preset.  If not, see <https://www.gnu.org/licenses/>.

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

(require 'cl-lib)

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

(defun eglot-python-preset-has-metadata-p (&optional file)
  "Return non-nil if FILE contains PEP-723 script metadata.
If FILE is nil, use the current buffer's file.
Scans first 2KB for `# /// script' ... `# ///' block."
  (let* ((file (or file (buffer-file-name)))
         (case-fold-search nil))
    (when file
      (save-match-data
        (with-temp-buffer
          (insert-file-contents-literally file nil 0 2048)
          (goto-char (point-min))
          (when (re-search-forward "^# /// script$" nil t)
            (re-search-forward "^# ///$" nil t)))))))

(defun eglot-python-preset--uv-env-dir ()
  "Return the uv script environments directory.
Uses `uv cache dir` to get the cache location, then appends environments-v2/."
  (let ((cache-dir (string-trim (shell-command-to-string "uv cache dir"))))
    (unless (string-empty-p cache-dir)
      (file-name-as-directory
       (expand-file-name "environments-v2" cache-dir)))))

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
        (message "[eglot-python-preset] uv couldn't find Python for %s" script-path)
        nil)
       ((and env-dir (string-prefix-p env-dir python-path))
        python-path)
       (t
        (display-warning
         'eglot-python-preset
         "Environment not synced. Run M-x eglot-python-preset-sync-environment"
         :warning)
        python-path)))))

(defun eglot-python-preset--python-env-dir (python-path)
  "Return the environment directory for PYTHON-PATH.
Given a path like /path/to/env/bin/python3, return /path/to/env/."
  (when python-path
    (let ((bin-dir (file-name-directory python-path)))
      (when (string-match-p "/bin/?$" bin-dir)
        (file-name-directory (directory-file-name bin-dir))))))

(defvar eglot-python-preset--workspace-configs (make-hash-table :test 'equal)
  "Hash table mapping directory paths to workspace configurations.
Used by basedpyright to look up script configurations for PEP-723.")

(defvar eglot-python-preset--original-workspace-configuration nil
  "Original value of `eglot-workspace-configuration' before we modified it.")

(defun eglot-python-preset--workspace-config-fn (server)
  "Return workspace configuration for the current `default-directory'.
Looks up configuration from `eglot-python-preset--workspace-configs'.
Falls back to original `eglot-workspace-configuration' for non-PEP-723 dirs."
  (let ((dir (file-name-as-directory (expand-file-name default-directory))))
    (or (gethash dir eglot-python-preset--workspace-configs)
        (when eglot-python-preset--original-workspace-configuration
          (if (functionp eglot-python-preset--original-workspace-configuration)
              (funcall eglot-python-preset--original-workspace-configuration server)
            eglot-python-preset--original-workspace-configuration)))))

(defun eglot-python-preset--init-options ()
  "Return initializationOptions for ty LSP server.
For PEP-723 scripts, includes environment configuration.
Only used for ty; basedpyright uses workspace configuration instead."
  (when (eq eglot-python-preset-lsp-server 'ty)
    (when-let* ((file (buffer-file-name))
                ((eglot-python-preset-has-metadata-p file))
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

(defun eglot-python-preset--setup-buffer ()
  "Configure Eglot settings for a PEP-723 script.
For basedpyright, registers configuration in `eglot-python-preset--workspace-configs'."
  (when (eq eglot-python-preset-lsp-server 'basedpyright)
    (when-let* ((file (buffer-file-name))
                ((eglot-python-preset-has-metadata-p file))
                (script-dir (file-name-as-directory
                             (expand-file-name (file-name-directory file))))
                (python-path (eglot-python-preset-get-python-path file)))
      (puthash script-dir
               `(:python (:pythonPath ,python-path))
               eglot-python-preset--workspace-configs))))

(defun eglot-python-preset--python-project-root-p (dir)
  "Return non-nil if DIR contains a Python project marker file."
  (seq-some (lambda (file)
              (file-exists-p (expand-file-name file dir)))
            eglot-python-preset-python-project-markers))

(defun eglot-python-preset--project-find (dir)
  "Project detection for Python files.
For PEP-723 scripts, returns (python-project . SCRIPT-DIR).
Otherwise, returns (python-project . ROOT) if DIR is inside a Python project."
  (cond
   ((and (memq major-mode '(python-mode python-ts-mode))
         (eglot-python-preset-has-metadata-p))
    (cons 'python-project (file-name-directory (buffer-file-name))))
   ((when-let* ((root (locate-dominating-file
                       dir #'eglot-python-preset--python-project-root-p)))
      (cons 'python-project root)))))

(cl-defmethod project-root ((project (head python-project)))
  (cdr project))

;;;###autoload
(defun eglot-python-preset-sync-environment ()
  "Sync the current PEP-723 script's environment, then restart Eglot.
Runs `uv sync --script' on the current file.
Uses shutdown + eglot-ensure instead of reconnect so that
initializationOptions are recomputed (needed for ty)."
  (interactive)
  (let ((script-path (buffer-file-name)))
    (unless script-path
      (user-error "No file associated with buffer"))
    (unless (executable-find "uv")
      (user-error "uv not found"))
    (unless (eglot-python-preset-has-metadata-p script-path)
      (user-error "Buffer does not contain PEP-723 metadata"))
    (let* ((default-directory (file-name-directory script-path))
           (status (call-process "uv" nil nil nil "sync" "--script"
                                 script-path)))
      (if (zerop status)
          (progn
            (message "Environment synced successfully")
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
    (unless (eglot-python-preset-has-metadata-p script-path)
      (user-error "Buffer does not contain PEP-723 metadata"))
    (let ((default-directory (file-name-directory script-path)))
      (compile (format "uv run %s" (shell-quote-argument script-path))))))

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
  ;; basedpyright needs workspace configuration via global function
  (when (eq eglot-python-preset-lsp-server 'basedpyright)
    (unless (eq (default-value 'eglot-workspace-configuration)
                #'eglot-python-preset--workspace-config-fn)
      (setq eglot-python-preset--original-workspace-configuration
            (default-value 'eglot-workspace-configuration))
      (setq-default eglot-workspace-configuration
                    #'eglot-python-preset--workspace-config-fn)))
  (add-hook 'python-mode-hook #'eglot-python-preset--setup-buffer t)
  (add-hook 'python-ts-mode-hook #'eglot-python-preset--setup-buffer t)
  (add-hook 'python-mode-hook #'eglot-ensure t)
  (add-hook 'python-ts-mode-hook #'eglot-ensure t))

(provide 'eglot-python-preset)
;;; eglot-python-preset.el ends here
