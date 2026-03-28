;;; test/test.el --- ERT tests for eglot-python-preset -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'project)
(require 'python)
(require 'wid-edit)
(require 'eglot-python-preset)

(defvar eglot-lsp-context nil)
(defvar eglot-server-programs nil)
(defvar eglot-workspace-configuration nil)
(defvar project-find-functions nil)
(defvar python-base-mode-hook nil)

(setq python-indent-guess-indent-offset-verbose nil)

(defun my-test--display-warning-fail (type message &optional _level _buffer-name)
  "Fail the current test for warning TYPE with MESSAGE."
  (ert-fail (format "Unexpected warning (%s): %s" type message)))

(advice-add 'display-warning :override #'my-test--display-warning-fail)

(defvar my-test-run-live-tests nil)
(defvar my-test-test-dir (file-name-directory load-file-name))
(defvar my-test-fixtures-dir
  (expand-file-name "fixtures/" my-test-test-dir))
(defvar my-test-argv-lsp-server
  (expand-file-name "argv-lsp-server.py" my-test-test-dir))
(defvar my-test-live-rass-client
  (expand-file-name "rass-live-client.py" my-test-test-dir))
(defvar my-test-rass-template-unit
  (expand-file-name "rass-template-unit.py" my-test-test-dir))
(defvar my-test-test-script-1
  (expand-file-name "pep-723-example-1.py" my-test-fixtures-dir))
(defvar my-test-test-script-2
  (expand-file-name "pep-723-example-2.py" my-test-fixtures-dir))

(defun my-test-write-executable (path)
  "Create an executable file at PATH."
  (with-temp-file path
    (insert "#!/bin/bash\nexit 0\n"))
  (set-file-modes path #o755))

(defun my-test-write-python-launcher (path target &optional args)
  "Create an executable launcher at PATH for Python script TARGET with ARGS."
  (with-temp-file path
    (insert "#!/bin/bash\n")
    (insert "exec python3 "
            (shell-quote-argument target))
    (dolist (arg args)
      (insert " " (shell-quote-argument arg)))
    (insert " \"$@\"\n"))
  (set-file-modes path #o755))

(defun my-test-file-contains-p (path substring)
  "Return non-nil if PATH contains SUBSTRING."
  (with-temp-buffer
    (insert-file-contents path)
    (search-forward substring nil t)))

(defun my-test-assert-file-contains-all (path strings)
  "Assert that PATH contains each string in STRINGS."
  (dolist (string strings)
    (should (my-test-file-contains-p path string))))

(defun my-test-assert-file-contains-none (path strings)
  "Assert that PATH contains none of STRINGS."
  (dolist (string strings)
    (should-not (my-test-file-contains-p path string))))

(defun my-test-write-script-metadata-file (path &optional body)
  "Write a PEP-723 script to PATH with optional BODY."
  (with-temp-file path
    (insert "# /// script\n")
    (insert "# dependencies = []\n")
    (insert "# ///\n\n")
    (insert (or body "print('hello')\n"))))

(defun my-test-fixture-content (name)
  "Return the contents of fixture file NAME."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name my-test-fixtures-dir))
    (buffer-string)))

(defun my-test-copy-fixture (name target-dir &optional target-name)
  "Copy fixture NAME into TARGET-DIR.  Return the new path.
If TARGET-NAME is non-nil, rename the file."
  (let ((src (expand-file-name name my-test-fixtures-dir))
        (dst (expand-file-name (or target-name name) target-dir)))
    (copy-file src dst t)
    dst))

(defun my-test-copy-fixture-dir (name target-dir)
  "Copy fixture subdirectory NAME into TARGET-DIR.  Return the new path."
  (let ((src (expand-file-name name my-test-fixtures-dir))
        (dst (expand-file-name name target-dir)))
    (copy-directory src dst nil t t)
    dst))

(defun my-test--with-tmp-dir (fn)
  "Call FN with a temporary directory, cleaned up afterward."
  (let ((tmp (make-temp-file "eglot-py-test-" t)))
    (unwind-protect
        (funcall fn tmp)
      (delete-directory tmp t))))

(defmacro my-test-with-tmp-dir (var &rest body)
  "Bind VAR to a temporary directory, evaluate BODY, then clean up."
  (declare (indent 1))
  `(my-test--with-tmp-dir (lambda (,var) ,@body)))

(defun my-test-with-file-buffer (file fn)
  "Visit FILE, call FN in that buffer, and kill the buffer afterward."
  (let ((buffer (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buffer
          (funcall fn))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun my-test-rass-contact (file tools)
  "Return the generated `rass` contact for FILE using TOOLS."
  (my-test-with-file-buffer
   file
   (lambda ()
     (let ((eglot-python-preset-lsp-server 'rass)
           (eglot-python-preset-rass-tools tools))
       (eglot-python-preset--server-contact nil)))))

(defun my-test-rass-preset-path (file tools)
  "Return the generated `rass` preset path for FILE using TOOLS."
  (cadr (my-test-rass-contact file tools)))

(defun my-test-rass-preset-name (file tools)
  "Return the generated `rass` preset filename for FILE using TOOLS."
  (file-name-nondirectory (my-test-rass-preset-path file tools)))

(defun my-test-rass-tools-type-matches-p (value)
  "Return non-nil if VALUE matches the custom type for `rass` tools."
  (widget-apply
   (widget-convert (get 'eglot-python-preset-rass-tools 'custom-type))
   :match
   value))

(defun my-test-rass-command-type-matches-p (value)
  "Return non-nil if VALUE matches the custom type for exact `rass` command."
  (widget-apply
   (widget-convert (get 'eglot-python-preset-rass-command 'custom-type))
   :match
   value))

(defun my-test-rass-tools-vector-widget ()
  "Return the literal command vector widget from the `rass` tools custom type."
  (let* ((type (widget-convert (get 'eglot-python-preset-rass-tools 'custom-type)))
         (choice (car (widget-get type :args))))
    (seq-find
     (lambda (widget)
       (equal "Command vector" (widget-get widget :tag)))
     (widget-get choice :args))))

(defun my-test-rass-command-widget ()
  "Return the exact `rass` command vector widget from the custom type."
  (let* ((type (widget-convert (get 'eglot-python-preset-rass-command 'custom-type)))
         (choice (car (last (widget-get type :args)))))
    choice))

(defun my-test-delete-script-env-if-present (script-path)
  "Delete the uv-managed script environment for SCRIPT-PATH, if present."
  (let* ((python-path (eglot-python-preset-get-python-path script-path))
         (env-dir (eglot-python-preset--python-env-dir python-path))
         (uv-env-dir (eglot-python-preset--uv-env-dir)))
    (when (and env-dir
               uv-env-dir
               (file-directory-p env-dir)
               (string-prefix-p uv-env-dir env-dir))
      (delete-directory env-dir t))))

(defun my-test-live-tests-enabled-p ()
  "Return non-nil when opt-in live tests should run."
  my-test-run-live-tests)

(defun my-test--run-rass-session (preset-path file-specs root-dir
                                              &optional timeout min-events)
  "Run rass-live-client with multiple files and return parsed result.
PRESET-PATH is the rass preset.  FILE-SPECS is a list of
\(FILE-PATH . LANGUAGE-ID) cons cells.  ROOT-DIR is the workspace
root.  TIMEOUT defaults to 8 seconds.  MIN-EVENTS is the minimum
publishDiagnostics events per file before settle starts (default 1)."
  (let* ((timeout (or timeout 8))
         (min-events (or min-events 1))
         (file-args (mapconcat
                     (lambda (spec)
                       (format "%s:%s"
                               (shell-quote-argument (car spec))
                               (shell-quote-argument (cdr spec))))
                     file-specs " "))
         (output (shell-command-to-string
                  (format
                   "python3 %s %s %s --root %s --timeout %s --min-events %s"
                   (shell-quote-argument my-test-live-rass-client)
                   (shell-quote-argument preset-path)
                   file-args
                   (shell-quote-argument root-dir)
                   timeout
                   min-events))))
    (json-parse-string output :object-type 'alist)))

(defun my-test--session-file-result (session-result file-path)
  "Extract per-file diagnostics from SESSION-RESULT for FILE-PATH."
  (let* ((files (alist-get 'files session-result))
         (uri (concat "file://" (expand-file-name file-path))))
    (alist-get (intern uri) files)))

(defun my-test--assert-file-diagnostics (session-result file-path
                                                        expected-codes
                                                        &optional expected-sources)
  "Assert FILE-PATH in SESSION-RESULT has exactly EXPECTED-CODES.
SESSION-RESULT is the parsed multi-file result.  EXPECTED-CODES is
compared as sorted deduplicated sets.  EXPECTED-SOURCES, when
non-nil, lists source patterns that must each match at least one
actual source."
  (should (alist-get 'initialized session-result))
  (let* ((file-data (my-test--session-file-result session-result file-path))
         (actual-codes (sort (delete-dups
                              (append (alist-get 'diagnosticCodes file-data) nil))
                             #'string<))
         (expected (sort (copy-sequence expected-codes) #'string<)))
    (should (equal actual-codes expected))
    (dolist (src-pat expected-sources)
      (should (cl-some (lambda (s) (string-match-p src-pat s))
                       (append (alist-get 'diagnosticSources file-data) nil))))))

(defun my-test--setup-fixture-dir (fixture-subdir tmp-dir)
  "Copy FIXTURE-SUBDIR contents into TMP-DIR."
  (let ((src-dir (expand-file-name fixture-subdir my-test-fixtures-dir)))
    (dolist (file (directory-files src-dir nil "\\`[^.]"))
      (let ((src (expand-file-name file src-dir))
            (dst (expand-file-name file tmp-dir)))
        (if (file-directory-p src)
            (copy-directory src dst nil t t)
          (copy-file src dst t))))))

(defun my-test-run-live-fake-ruff-client (tools)
  "Run the live `rass` client against a fake local Ruff using TOOLS."
  (let ((project-dir (make-temp-file "ruff-live" t)))
    (unwind-protect
        (let* ((python-file (expand-file-name "main.py" project-dir))
               (pyproject-file (expand-file-name "pyproject.toml" project-dir))
               (venv-bin-dir (expand-file-name ".venv/bin" project-dir))
               (ruff-path (expand-file-name "ruff" venv-bin-dir))
               (preset-path nil))
          (with-temp-file python-file
            (insert "import os\n"))
          (with-temp-file pyproject-file
            (insert "[project]\n")
            (insert "name = \"ruff-live-test\"\n")
            (insert "version = \"0.0.0\"\n"))
          (make-directory venv-bin-dir t)
          (my-test-write-python-launcher
           ruff-path my-test-argv-lsp-server '("--server-name" "ruff"))
          (setq preset-path (my-test-rass-preset-path python-file tools))
          (my-test--run-rass-session
           preset-path
           `((,python-file . "python"))
           project-dir))
      (delete-directory project-dir t))))

(defun my-test-run-live-fake-ty-ruff-client (tools)
  "Run the live `rass` client against fake local Ty and Ruff using TOOLS."
  (let ((project-dir (make-temp-file "ty-ruff-live" t)))
    (unwind-protect
        (let* ((python-file (expand-file-name "script.py" project-dir))
               (venv-bin-dir (expand-file-name ".venv/bin" project-dir))
               (ruff-path (expand-file-name "ruff" venv-bin-dir))
               (ty-path (expand-file-name "ty" venv-bin-dir))
               (preset-path nil))
          (make-directory venv-bin-dir t)
          (my-test-write-script-metadata-file python-file "import requests\n")
          (my-test-write-python-launcher
           ruff-path my-test-argv-lsp-server '("--server-name" "ruff"))
          (my-test-write-python-launcher
           ty-path my-test-argv-lsp-server '("--server-name" "ty"))
          (cl-letf (((symbol-function 'eglot-python-preset-get-python-path)
                     (lambda (_file-path) "/tmp/fake-env/bin/python3")))
            (setq preset-path (my-test-rass-preset-path python-file tools)))
          (my-test--run-rass-session
           preset-path
           `((,python-file . "python"))
           project-dir))
      (delete-directory project-dir t))))

(defun my-test-assert-diagnostics (result expected-codes
                                          &optional expected-sources)
  "Assert single-file RESULT has exactly EXPECTED-CODES diagnostics.
RESULT is a multi-file session result with one file entry.
EXPECTED-CODES is compared as sorted deduplicated sets.
EXPECTED-SOURCES, when non-nil, lists source patterns that must
each match at least one actual source."
  (should (alist-get 'initialized result))
  (let* ((files (alist-get 'files result))
         (file-data (cdar files)))
    (let ((actual (sort (delete-dups
                         (append (alist-get 'diagnosticCodes file-data) nil))
                        #'string<))
          (expected (sort (copy-sequence expected-codes) #'string<)))
      (should (equal actual expected)))
    (dolist (src-pat expected-sources)
      (should (cl-some (lambda (s) (string-match-p src-pat s))
                       (append (alist-get 'diagnosticSources file-data) nil))))))

(defun my-test-run-rass-template-unit (preset-path)
  "Run the Python template unit helper against PRESET-PATH."
  (with-temp-buffer
    (let ((status (call-process "python3" nil (current-buffer) nil
                                my-test-rass-template-unit
                                preset-path)))
      (unless (zerop status)
        (error "Template unit helper failed: %s" (string-trim (buffer-string))))
      (goto-char (point-min))
      (json-parse-string (buffer-string)
                         :object-type 'alist
                         :array-type 'list
                         :null-object nil
                         :false-object nil))))

(ert-deftest eglot-python-preset-detects-metadata-example-1 ()
  (my-test-with-file-buffer
   my-test-test-script-1
   (lambda ()
     (should (eglot-python-preset-has-metadata-p)))))

(ert-deftest eglot-python-preset-detects-metadata-example-2 ()
  (my-test-with-file-buffer
   my-test-test-script-2
   (lambda ()
     (should (eglot-python-preset-has-metadata-p)))))

(ert-deftest eglot-python-preset-detects-absence-of-metadata ()
  (let ((regular-py (make-temp-file "test" nil ".py" "print('hello')\n")))
    (unwind-protect
        (my-test-with-file-buffer
         regular-py
         (lambda ()
           (should-not (eglot-python-preset-has-metadata-p))))
      (delete-file regular-py))))

(ert-deftest eglot-python-preset-detects-incomplete-metadata-block ()
  (let ((incomplete-py
         (make-temp-file "test" nil ".py" "# /// script\n# no closing\n")))
    (unwind-protect
        (my-test-with-file-buffer
         incomplete-py
         (lambda ()
           (should-not (eglot-python-preset-has-metadata-p))))
      (delete-file incomplete-py))))

(ert-deftest eglot-python-preset-pep723-context-is-nil-for-regular-file ()
  (let ((regular-py (make-temp-file "test" nil ".py" "print('hello')\n")))
    (unwind-protect
        (my-test-with-file-buffer
         regular-py
         (lambda ()
           (should-not (eglot-python-preset--pep-723-context))))
      (delete-file regular-py))))

(ert-deftest eglot-python-preset-uv-python-find-for-synced-script ()
  (skip-unless (executable-find "uv"))
  (let ((project-dir (make-temp-file "uv-script" t)))
    (unwind-protect
        (let* ((python-file (expand-file-name "script.py" project-dir))
               (default-directory project-dir))
          (my-test-write-script-metadata-file python-file)
          (should (zerop (call-process "uv" nil nil nil "sync" "--script" python-file)))
          (let ((python-path (eglot-python-preset-get-python-path python-file)))
            (should python-path)
            (should (file-exists-p python-path))))
      (delete-directory project-dir t))))

(ert-deftest eglot-python-preset-ty-init-options-use-pep723-context ()
  (my-test-with-file-buffer
   my-test-test-script-1
   (lambda ()
     (cl-letf (((symbol-function 'eglot-python-preset-get-python-path)
                (lambda (_script-path) "/tmp/fake-env/bin/python3")))
       (let ((eglot-python-preset-lsp-server 'ty))
         (should (equal
                  `(:configuration
                    (:environment
                     (:python "/tmp/fake-env/"
                      :root [,(file-name-directory my-test-test-script-1)])))
                  (eglot-python-preset--init-options))))))))

(ert-deftest eglot-python-preset-ty-contact-prefers-local-venv ()
  (let ((eglot-python-preset-lsp-server 'ty)
        (project-dir (make-temp-file "test-project" t)))
    (unwind-protect
        (let* ((venv-bin-dir (expand-file-name ".venv/bin" project-dir))
               (ty-path (expand-file-name "ty" venv-bin-dir))
               (python-file (expand-file-name "main.py" project-dir)))
          (make-directory venv-bin-dir t)
          (with-temp-file (expand-file-name "pyproject.toml" project-dir))
          (my-test-write-executable ty-path)
          (with-temp-file python-file
            (insert "print('hello')\n"))
          (my-test-with-file-buffer
           python-file
           (lambda ()
             (should (equal (list ty-path "server")
                            (eglot-python-preset--server-contact nil))))))
      (delete-directory project-dir t))))

(ert-deftest eglot-python-preset-basedpyright-contact-prefers-local-venv ()
  (let ((eglot-python-preset-lsp-server 'basedpyright)
        (project-dir (make-temp-file "test-project" t)))
    (unwind-protect
        (let* ((venv-bin-dir (expand-file-name ".venv/bin" project-dir))
               (langserver-path (expand-file-name "basedpyright-langserver"
                                                  venv-bin-dir))
               (python-file (expand-file-name "main.py" project-dir)))
          (make-directory venv-bin-dir t)
          (with-temp-file (expand-file-name "pyproject.toml" project-dir))
          (my-test-write-executable langserver-path)
          (with-temp-file python-file
            (insert "print('hello')\n"))
          (my-test-with-file-buffer
           python-file
           (lambda ()
             (should (equal (list langserver-path "--stdio")
                            (eglot-python-preset--server-contact nil))))))
      (delete-directory project-dir t))))

(ert-deftest eglot-python-preset-pep723-script-prefers-local-ty-from-script-root ()
  (let ((eglot-python-preset-lsp-server 'ty)
        (project-dir (make-temp-file "test-project" t)))
    (unwind-protect
        (let* ((venv-bin-dir (expand-file-name ".venv/bin" project-dir))
               (ty-path (expand-file-name "ty" venv-bin-dir))
               (python-file (expand-file-name "main.py" project-dir)))
          (make-directory venv-bin-dir t)
          (my-test-write-executable ty-path)
          (my-test-write-script-metadata-file python-file)
          (my-test-with-file-buffer
           python-file
           (lambda ()
             (cl-letf (((symbol-function 'eglot-python-preset--init-options)
                        (lambda () nil)))
               (should (equal (list ty-path "server")
                              (eglot-python-preset--server-contact nil)))))))
      (delete-directory project-dir t))))

(ert-deftest eglot-python-preset-basedpyright-workspace-config-merges-python-path ()
  (let ((eglot-python-preset-lsp-server 'basedpyright)
        (user-config '(:basedpyright.analysis (:typeCheckingMode "strict"))))
    (my-test-with-file-buffer
     my-test-test-script-1
     (lambda ()
       (let ((mock-server (list :managed-buffers (list (current-buffer)))))
         (cl-letf (((symbol-function 'eglot--managed-buffers)
                    (lambda (_s) (plist-get mock-server :managed-buffers)))
                   ((symbol-function 'eglot-python-preset-get-python-path)
                    (lambda (_file-path) "/tmp/fake-env/bin/python3")))
           (let ((config
                  (eglot-python-preset--workspace-configuration-plist-a
                   (lambda (_server &optional _path) user-config)
                   mock-server
                   my-test-test-dir)))
             (should (equal "/tmp/fake-env/bin/python3"
                            (plist-get (plist-get config :python) :pythonPath)))
             (should (equal "strict"
                             (plist-get (plist-get config :basedpyright.analysis)
                                        :typeCheckingMode))))))))))

(ert-deftest eglot-python-preset-merge-plists-recursively-merges-values ()
  (let ((merged
         (eglot-python-preset--merge-plists
          '(:python (:venvPath "/tmp/venv")
            :basedpyright.analysis (:diagnosticMode "openFilesOnly"))
          '(:python (:pythonPath "/tmp/python")
            :basedpyright.analysis (:typeCheckingMode "strict")))))
    (should (equal "/tmp/venv"
                   (plist-get (plist-get merged :python) :venvPath)))
    (should (equal "/tmp/python"
                   (plist-get (plist-get merged :python) :pythonPath)))
    (should (equal "openFilesOnly"
                   (plist-get (plist-get merged :basedpyright.analysis)
                              :diagnosticMode)))
    (should (equal "strict"
                   (plist-get (plist-get merged :basedpyright.analysis)
                              :typeCheckingMode)))))

(ert-deftest eglot-python-preset-workspace-config-without-managed-buffers-returns-base ()
  (let ((eglot-python-preset-lsp-server 'basedpyright)
        (base-config '(:some (:setting "value"))))
    (let ((mock-server (list :managed-buffers nil)))
      (cl-letf (((symbol-function 'eglot--managed-buffers)
                 (lambda (_s) (plist-get mock-server :managed-buffers))))
        (should (equal base-config
                       (eglot-python-preset--workspace-configuration-plist-a
                        (lambda (_server &optional _path) base-config)
                        mock-server
                        my-test-test-dir)))))))

(ert-deftest eglot-python-preset-non-pep723-file-returns-base-config ()
  (let ((eglot-python-preset-lsp-server 'basedpyright)
        (regular-py (make-temp-file "test" nil ".py" "print('hello')\n")))
    (unwind-protect
        (my-test-with-file-buffer
         regular-py
         (lambda ()
           (let* ((base-config '(:basedpyright (:setting "value")))
                  (mock-server (list :managed-buffers (list (current-buffer)))))
             (cl-letf (((symbol-function 'eglot--managed-buffers)
                        (lambda (_s) (plist-get mock-server :managed-buffers))))
               (should (equal base-config
                              (eglot-python-preset--workspace-configuration-plist-a
                               (lambda (_server &optional _path) base-config)
                               mock-server
                               nil)))))))
      (delete-file regular-py))))

(ert-deftest eglot-python-preset-non-basedpyright-server-returns-base-config ()
  (let ((eglot-python-preset-lsp-server 'ty))
    (my-test-with-file-buffer
     my-test-test-script-1
     (lambda ()
       (let* ((base-config '(:some (:config "value")))
              (mock-server (list :managed-buffers (list (current-buffer)))))
         (cl-letf (((symbol-function 'eglot--managed-buffers)
                    (lambda (_s) (plist-get mock-server :managed-buffers))))
           (should (equal base-config
                          (eglot-python-preset--workspace-configuration-plist-a
                           (lambda (_server &optional _path) base-config)
                           mock-server
                           my-test-test-dir)))))))))

(ert-deftest eglot-python-preset-sync-and-remove-environment ()
  (skip-unless (executable-find "uv"))
  (let ((project-dir (make-temp-file "sync-script" t)))
    (unwind-protect
          (let* ((python-file (expand-file-name "script.py" project-dir))
                 (default-directory project-dir))
          (my-test-write-script-metadata-file python-file)
          (should (zerop (call-process "uv" nil nil nil "sync" "--script" python-file)))
          (let* ((python-path (eglot-python-preset-get-python-path python-file))
                 (env-dir (eglot-python-preset--python-env-dir python-path))
                 (uv-env-dir (eglot-python-preset--uv-env-dir)))
            (should python-path)
            (should env-dir)
            (should (file-directory-p env-dir))
            (should (string-prefix-p uv-env-dir env-dir))
            (delete-directory env-dir t)
            (should-not (file-directory-p env-dir))))
      (delete-directory project-dir t))))

(ert-deftest eglot-python-preset-rass-contact-generates-local-ty-preset ()
  (let ((project-dir (make-temp-file "rass-ty" t)))
    (unwind-protect
        (let* ((venv-bin-dir (expand-file-name ".venv/bin" project-dir))
               (ty-path (expand-file-name "ty" venv-bin-dir))
               (python-file (expand-file-name "main.py" project-dir))
               (fake-python "/tmp/rass-ty/bin/python3"))
          (make-directory venv-bin-dir t)
          (my-test-write-executable ty-path)
          (my-test-write-script-metadata-file python-file)
          (cl-letf (((symbol-function 'eglot-python-preset-get-python-path)
                     (lambda (_file-path) fake-python)))
            (let ((preset-path (my-test-rass-preset-path python-file '(ty))))
              (should (file-exists-p preset-path))
              (my-test-assert-file-contains-all
               preset-path
               (list ty-path
                     "\"configuration\""
                     "\"python\":\"/tmp/rass-ty/\""
                     "BASEDPYRIGHT_CONFIGURATION: dict[str, Any] | None = None")))))
      (delete-directory project-dir t))))

(ert-deftest eglot-python-preset-rass-default-tools-include-ty-init-config ()
  (let ((project-dir (make-temp-file "rass-ty-ruff" t)))
    (unwind-protect
        (let* ((python-file (expand-file-name "script.py" project-dir))
               (preset-path nil))
          (my-test-write-script-metadata-file python-file)
          (cl-letf (((symbol-function 'eglot-python-preset-get-python-path)
                     (lambda (_file-path) "/tmp/fake-env/bin/python3"))
                    ((symbol-function 'eglot-python-preset--resolve-executable)
                     (lambda (name)
                       (pcase name
                         ("ruff" "/usr/bin/ruff")
                         ("ty" "/usr/bin/ty")
                         (_ name)))))
            (setq preset-path (my-test-rass-preset-path python-file '(ty ruff))))
          (my-test-assert-file-contains-all
           preset-path
           (list "\"ruff\""
                 "\"ty\""
                 "if method == \"initialize\" and TY_CONFIGURATION:"
                 "\"python\":\"/tmp/fake-env/\"")))
      (delete-directory project-dir t))))

(ert-deftest eglot-python-preset-rass-reuses-shared-preset-for-global-tools ()
  (let ((python-file-1 (make-temp-file "rass-shared-a" nil ".py" "print('a')\n"))
        (python-file-2 (make-temp-file "rass-shared-b" nil ".py" "print('b')\n")))
    (unwind-protect
        (cl-letf (((symbol-function 'eglot-python-preset--resolve-executable)
                   (lambda (name)
                     (pcase name
                       ("ruff" "/usr/bin/ruff")
                       ("ty" "/usr/bin/ty")
                       (_ name)))))
          (let ((preset-name-1 (my-test-rass-preset-name python-file-1 '(ruff ty)))
                (preset-name-2 (my-test-rass-preset-name python-file-2 '(ruff ty))))
            (should (equal preset-name-1 preset-name-2))
            (should (string-prefix-p "rass-preset-shared-" preset-name-1))))
      (delete-file python-file-1)
      (delete-file python-file-2))))

(ert-deftest eglot-python-preset-rass-command-override-bypasses-generated-preset ()
  (let ((python-file (make-temp-file "rass-command" nil ".py" "print('hello')\n")))
    (unwind-protect
        (my-test-with-file-buffer
         python-file
         (lambda ()
           (let ((eglot-python-preset-lsp-server 'rass)
                 (eglot-python-preset-rass-command ["rass" "python"]))
             (cl-letf (((symbol-function 'eglot-python-preset--rass-preset-path)
                        (lambda ()
                          (error "generated preset should not be used"))))
               (should (equal '("rass" "python")
                              (eglot-python-preset--server-contact nil)))))))
      (delete-file python-file))))

(ert-deftest eglot-python-preset-rass-uses-contextual-preset-for-local-venv-tools ()
  (let ((project-dir-1 (make-temp-file "rass-local-a" t))
        (project-dir-2 (make-temp-file "rass-local-b" t)))
    (unwind-protect
        (let* ((venv-bin-dir-1 (expand-file-name ".venv/bin" project-dir-1))
               (venv-bin-dir-2 (expand-file-name ".venv/bin" project-dir-2))
               (ty-path-1 (expand-file-name "ty" venv-bin-dir-1))
               (ty-path-2 (expand-file-name "ty" venv-bin-dir-2))
               (python-file-1 (expand-file-name "main.py" project-dir-1))
               (python-file-2 (expand-file-name "main.py" project-dir-2))
               (preset-name-1 nil)
               (preset-name-2 nil))
          (make-directory venv-bin-dir-1 t)
          (make-directory venv-bin-dir-2 t)
          (with-temp-file (expand-file-name "pyproject.toml" project-dir-1))
          (with-temp-file (expand-file-name "pyproject.toml" project-dir-2))
          (my-test-write-executable ty-path-1)
          (my-test-write-executable ty-path-2)
          (with-temp-file python-file-1
            (insert "print('a')\n"))
          (with-temp-file python-file-2
            (insert "print('b')\n"))
          (setq preset-name-1 (my-test-rass-preset-name python-file-1 '(ty)))
          (setq preset-name-2 (my-test-rass-preset-name python-file-2 '(ty)))
          (should-not (equal preset-name-1 preset-name-2))
          (should (string-prefix-p "rass-preset-contextual-" preset-name-1))
          (should (string-prefix-p "rass-preset-contextual-" preset-name-2)))
      (delete-directory project-dir-1 t)
      (delete-directory project-dir-2 t))))

(ert-deftest eglot-python-preset-rass-uses-contextual-preset-for-pep723-ty-config ()
  (let ((project-dir-1 (make-temp-file "rass-pep-a" t))
        (project-dir-2 (make-temp-file "rass-pep-b" t)))
    (unwind-protect
        (let* ((python-file-1 (expand-file-name "script.py" project-dir-1))
               (python-file-2 (expand-file-name "script.py" project-dir-2))
               (preset-name-1 nil)
               (preset-name-2 nil))
          (my-test-write-script-metadata-file python-file-1)
          (my-test-write-script-metadata-file python-file-2)
          (cl-letf (((symbol-function 'eglot-python-preset-get-python-path)
                     (lambda (_file-path) "/tmp/fake-env/bin/python3")))
            (setq preset-name-1 (my-test-rass-preset-name python-file-1 '(ty)))
            (setq preset-name-2 (my-test-rass-preset-name python-file-2 '(ty))))
          (should-not (equal preset-name-1 preset-name-2))
          (should (string-prefix-p "rass-preset-contextual-" preset-name-1))
          (should (string-prefix-p "rass-preset-contextual-" preset-name-2)))
      (delete-directory project-dir-1 t)
      (delete-directory project-dir-2 t))))

(ert-deftest eglot-python-preset-rass-preserves-basedpyright-symbol-config ()
  (let ((project-dir (make-temp-file "rass-bp" t)))
    (unwind-protect
        (let* ((venv-bin-dir (expand-file-name ".venv/bin" project-dir))
               (langserver-path (expand-file-name "basedpyright-langserver"
                                                  venv-bin-dir))
               (python-file (expand-file-name "main.py" project-dir))
               (fake-python "/tmp/rass-bp/bin/python3"))
          (make-directory venv-bin-dir t)
          (my-test-write-executable langserver-path)
          (my-test-write-script-metadata-file python-file)
          (cl-letf (((symbol-function 'eglot-python-preset-get-python-path)
                     (lambda (_file-path) fake-python)))
            (let ((preset-path (my-test-rass-preset-path python-file '(basedpyright))))
              (should (file-exists-p preset-path))
              (my-test-assert-file-contains-all
               preset-path
               (list langserver-path
                     "\"pythonPath\":\"/tmp/rass-bp/bin/python3\""
                     "TY_CONFIGURATION: dict[str, Any] | None = None")))))
      (delete-directory project-dir t))))

(ert-deftest eglot-python-preset-rass-preserves-basedpyright-and-ruff-config ()
  (let ((project-dir (make-temp-file "rass-bp-ruff" t)))
    (unwind-protect
        (let* ((venv-bin-dir (expand-file-name ".venv/bin" project-dir))
               (langserver-path (expand-file-name "basedpyright-langserver"
                                                  venv-bin-dir))
               (ruff-path (expand-file-name "ruff" venv-bin-dir))
               (python-file (expand-file-name "main.py" project-dir))
               (fake-python "/tmp/rass-bp-ruff/bin/python3"))
          (make-directory venv-bin-dir t)
          (my-test-write-executable langserver-path)
          (my-test-write-executable ruff-path)
          (my-test-write-script-metadata-file python-file)
          (cl-letf (((symbol-function 'eglot-python-preset-get-python-path)
                     (lambda (_file-path) fake-python)))
            (let ((preset-path (my-test-rass-preset-path python-file '(basedpyright ruff))))
              (should (file-exists-p preset-path))
              (my-test-assert-file-contains-all
               preset-path
               (list langserver-path
                     ruff-path
                     "\"pythonPath\":\"/tmp/rass-bp-ruff/bin/python3\""
                     "TY_CONFIGURATION: dict[str, Any] | None = None")))))
      (delete-directory project-dir t))))

(ert-deftest eglot-python-preset-rass-template-helper-unit-checks-pass ()
  (let ((python-file (make-temp-file "rass-template" nil ".py" "print('hello')\n")))
    (unwind-protect
        (let* ((preset-path (my-test-rass-preset-path python-file '(ruff ty)))
               (result (my-test-run-rass-template-unit preset-path))
               (server-kind (alist-get 'serverKind result))
               (non-list-payload (alist-get 'nonListPayload result))
               (payload (alist-get 'responsePayload result)))
          (should (equal "basedpyright" (alist-get 'basedpyright server-kind nil nil #'equal)))
          (should (equal "basedpyright" (alist-get 'basedpyright-langserver server-kind nil nil #'equal)))
          (should (equal "ruff" (alist-get 'ruff server-kind nil nil #'equal)))
          (should (equal "ty" (alist-get 'ty server-kind nil nil #'equal)))
          (should-not (alist-get 'unknown server-kind))
          (should (equal t (alist-get 'unchanged non-list-payload)))
          (should (equal t (alist-get 'existing (nth 0 payload))))
          (should (equal "/tmp/env/"
                         (alist-get 'python
                                    (alist-get 'environment
                                               (alist-get 'configuration (nth 0 payload))))))
          (should (equal "/tmp/env/"
                         (alist-get 'python
                                    (alist-get 'environment
                                               (alist-get 'configuration (nth 1 payload)))))))
      (delete-file python-file))))

(ert-deftest eglot-python-preset-rass-preserves-practical-ruff-vector-args ()
  (let ((python-file (make-temp-file "rass-ruff" nil ".py" "print('hello')\n")))
    (unwind-protect
        (let ((preset-path
               (my-test-rass-preset-path python-file '(["ruff" "--isolated" "server"]))))
          (should (file-exists-p preset-path))
          (my-test-assert-file-contains-all
           preset-path
           (list "\"ruff\"" "\"--isolated\"" "\"server\"")))
      (delete-file python-file))))

(ert-deftest eglot-python-preset-rass-preserves-unsupported-tool-args ()
  (let ((python-file (make-temp-file "rass-custom" nil ".py" "print('hello')\n")))
    (unwind-protect
        (let ((preset-path
               (my-test-rass-preset-path
                python-file
                '(["custom-lsp" "--stdio" "--flag" "value"]))))
          (should (file-exists-p preset-path))
          (my-test-assert-file-contains-all
           preset-path
           (list "\"custom-lsp\"" "\"--stdio\"" "\"--flag\"" "\"value\"")))
      (delete-file python-file))))

(ert-deftest eglot-python-preset-rass-reuses-shared-preset-for-unsupported-vector-tools ()
  (let ((python-file-1 (make-temp-file "rass-custom-a" nil ".py" "print('a')\n"))
        (python-file-2 (make-temp-file "rass-custom-b" nil ".py" "print('b')\n"))
        (tools '(["custom-lsp" "--stdio" "--flag" "value"])))
    (unwind-protect
        (let ((preset-name-1 (my-test-rass-preset-name python-file-1 tools))
              (preset-name-2 (my-test-rass-preset-name python-file-2 tools)))
          (should (equal preset-name-1 preset-name-2))
          (should (string-prefix-p "rass-preset-shared-" preset-name-1)))
      (delete-file python-file-1)
      (delete-file python-file-2))))

(ert-deftest eglot-python-preset-rass-write-file-if-changed-preserves-mtime ()
  (let ((path (make-temp-file "rass-write")))
    (unwind-protect
        (progn
          (eglot-python-preset--write-file-if-changed path "one")
          (set-file-times path (seconds-to-time 1000))
          (eglot-python-preset--write-file-if-changed path "one")
          (should (equal (seconds-to-time 1000)
                         (file-attribute-modification-time
                          (file-attributes path))))
          (eglot-python-preset--write-file-if-changed path "two")
          (should (string= "two"
                           (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string)))))
      (delete-file path))))

(ert-deftest eglot-python-preset-rass-contextual-cleanup-limits-files ()
  (let ((generated-dir (make-temp-file "rass-generated" t)))
    (unwind-protect
        (let ((eglot-python-preset-rass-max-contextual-presets 2))
          (cl-letf (((symbol-function 'eglot-python-preset--rass-generated-dir)
                     (lambda () generated-dir)))
            (let ((paths (mapcar (lambda (name)
                                   (expand-file-name name generated-dir))
                                 '("rass-preset-contextual-1.py"
                                   "rass-preset-contextual-2.py"
                                   "rass-preset-contextual-3.py"))))
              (cl-loop for path in paths
                       for seconds from 1000
                       do (with-temp-file path
                            (insert path))
                       do (set-file-times path (seconds-to-time seconds)))
              (eglot-python-preset--cleanup-rass-contextual-presets (nth 0 paths))
              (should (file-exists-p (nth 0 paths)))
              (should (file-exists-p (nth 2 paths)))
              (should-not (file-exists-p (nth 1 paths))))))
      (delete-directory generated-dir t))))

(ert-deftest eglot-python-preset-rass-contextual-cleanup-disabled-when-nil ()
  (let ((generated-dir (make-temp-file "rass-generated" t)))
    (unwind-protect
        (let ((eglot-python-preset-rass-max-contextual-presets nil))
          (cl-letf (((symbol-function 'eglot-python-preset--rass-generated-dir)
                     (lambda () generated-dir)))
            (let ((paths (mapcar (lambda (name)
                                   (expand-file-name name generated-dir))
                                 '("rass-preset-contextual-1.py"
                                   "rass-preset-contextual-2.py"
                                   "rass-preset-contextual-3.py"))))
              (dolist (path paths)
                (with-temp-file path
                  (insert path)))
              (eglot-python-preset--cleanup-rass-contextual-presets (car paths))
              (should (seq-every-p #'file-exists-p paths)))))
      (delete-directory generated-dir t))))

(ert-deftest eglot-python-preset-template-cache-refreshes-when-file-changes ()
  (let ((library-dir (make-temp-file "rass-library" t)))
    (unwind-protect
        (let* ((template-dir (expand-file-name "templates" library-dir))
               (template-path (expand-file-name "sample.tpl.py" template-dir)))
          (make-directory template-dir t)
          (with-temp-file template-path
            (insert "first"))
          (set-file-times template-path (seconds-to-time 1000))
          (clrhash eglot-python-preset--template-cache)
          (cl-letf (((symbol-function 'eglot-python-preset--library-dir)
                     (lambda () library-dir)))
            (should (string= "first"
                             (eglot-python-preset--template-string "sample.tpl.py")))
            (with-temp-file template-path
              (insert "second"))
            (set-file-times template-path (seconds-to-time 2000))
            (should (string= "second"
                             (eglot-python-preset--template-string "sample.tpl.py")))))
      (delete-directory library-dir t))))

(ert-deftest eglot-python-preset-rass-tools-custom-type-accepts-symbols-and-vectors ()
  (should (my-test-rass-tools-type-matches-p '(ty ruff)))
  (should (my-test-rass-tools-type-matches-p '(ty ["custom-lsp" "--stdio"]))))

(ert-deftest eglot-python-preset-rass-command-custom-type-accepts-nil-and-vectors ()
  (should (my-test-rass-command-type-matches-p nil))
  (should (my-test-rass-command-type-matches-p ["rass" "python"])))

(ert-deftest eglot-python-preset-rass-command-custom-type-rejects-bad-vector ()
  (should-not (my-test-rass-command-type-matches-p ["rass" 1])))

(ert-deftest eglot-python-preset-rass-command-widget-default-is-valid ()
  (let ((widget (my-test-rass-command-widget)))
    (should widget)
    (should (equal "[\"rass\" \"python\"]" (widget-get widget :value)))
    (should (widget-apply widget :match ["rass" "/tmp/preset.py"]))
    (should-not (widget-apply widget :match ["rass" 1]))))

(ert-deftest eglot-python-preset-rass-tools-vector-widget-default-is-valid ()
  (let ((widget (my-test-rass-tools-vector-widget)))
    (should widget)
    (should (equal "[\"command\"]" (widget-get widget :value)))
    (should (widget-apply widget :match ["custom-lsp" "--stdio"]))
    (should-not (widget-apply widget :match nil))))

(ert-deftest eglot-python-preset-rass-tool-label-handles-edge-cases ()
  (should (equal "tool"
                 (eglot-python-preset--rass-tool-label [""])))
  (should (string-prefix-p
           "my-tool-argv-"
           (eglot-python-preset--rass-tool-label ["/tmp/My Tool!.exe" "--stdio"]))))

(ert-deftest eglot-python-preset-rass-tools-custom-type-rejects-bad-vector ()
  (should-not (my-test-rass-tools-type-matches-p '(["custom-lsp" 1]))))

(ert-deftest eglot-python-preset-rass-rejects-literal-command-lists ()
  (should-not (my-test-rass-tools-type-matches-p '(("custom-lsp" "--stdio"))))
  (should-error
   (let ((python-file (make-temp-file "rass-list" nil ".py" "print('hello')\n")))
     (unwind-protect
         (my-test-rass-preset-path python-file '(("custom-lsp" "--stdio")))
       (delete-file python-file)))
   :type 'error))

(ert-deftest eglot-python-preset-lsp-server-safe-local-variable ()
  (should (eglot-python-preset--lsp-server-safe-p 'ty))
  (should (eglot-python-preset--lsp-server-safe-p 'basedpyright))
  (should (eglot-python-preset--lsp-server-safe-p 'rass))
  (should-not (eglot-python-preset--lsp-server-safe-p 'unknown))
  (should-not (eglot-python-preset--lsp-server-safe-p "ty"))
  (should-not (eglot-python-preset--lsp-server-safe-p nil)))

(ert-deftest eglot-python-preset-rass-tools-safe-local-variable ()
  (should (eglot-python-preset--rass-tools-safe-p '(ty ruff)))
  (should (eglot-python-preset--rass-tools-safe-p '(ty ruff basedpyright)))
  (should (eglot-python-preset--rass-tools-safe-p '()))
  (should-not (eglot-python-preset--rass-tools-safe-p '(ty ["ruff" "server"])))
  (should-not (eglot-python-preset--rass-tools-safe-p '(unknown)))
  (should-not (eglot-python-preset--rass-tools-safe-p "ty")))

(ert-deftest eglot-python-preset-python-modes-safe-local-variable ()
  (should (eglot-python-preset--modes-safe-p
           '(python-mode python-ts-mode)))
  (should (eglot-python-preset--modes-safe-p '()))
  (should-not (eglot-python-preset--modes-safe-p '("python-mode")))
  (should-not (eglot-python-preset--modes-safe-p "python-mode")))

(ert-deftest eglot-python-preset-all-managed-modes ()
  (should (equal eglot-python-preset-python-modes
                 (eglot-python-preset--all-managed-modes))))

(ert-deftest eglot-python-preset-project-markers-safe-local-variable ()
  (should (eglot-python-preset--project-markers-safe-p
           '("pyproject.toml" "requirements.txt")))
  (should (eglot-python-preset--project-markers-safe-p '()))
  (should-not (eglot-python-preset--project-markers-safe-p '(pyproject.toml)))
  (should-not (eglot-python-preset--project-markers-safe-p "pyproject.toml")))

(ert-deftest eglot-python-preset-project-find-with-lsp-context ()
  "Project find returns python-project when eglot-lsp-context is set."
  (my-test-with-tmp-dir tmp-dir
    (let* ((project-dir (expand-file-name "myproject/" tmp-dir))
           (src-dir (expand-file-name "src/" project-dir))
           (eglot-lsp-context t)
           (major-mode 'python-mode))
      (make-directory src-dir t)
      (with-temp-file (expand-file-name "pyproject.toml" project-dir)
        (insert "[project]\nname = \"test\"\n"))
      (let ((result (eglot-python-preset--project-find src-dir)))
        (should result)
        (should (eq (car result) 'python-project))
        (should (string= (cdr result)
                          (file-name-as-directory project-dir)))))))

(ert-deftest eglot-python-preset-project-find-without-lsp-context ()
  "Project find returns nil when eglot-lsp-context is not set."
  (my-test-with-tmp-dir tmp-dir
    (let* ((project-dir (expand-file-name "myproject/" tmp-dir))
           (src-dir (expand-file-name "src/" project-dir))
           (eglot-lsp-context nil))
      (make-directory src-dir t)
      (with-temp-file (expand-file-name "pyproject.toml" project-dir)
        (insert "[project]\nname = \"test\"\n"))
      (should-not (eglot-python-preset--project-find src-dir)))))

(ert-deftest eglot-python-preset-project-find-ignores-non-python-modes ()
  "Return nil for non-Python major modes even when Python markers exist."
  (my-test-with-tmp-dir tmp-dir
    (let* ((project-dir (expand-file-name "myproject/" tmp-dir))
           (src-dir (expand-file-name "src/" project-dir))
           (eglot-lsp-context t))
      (make-directory src-dir t)
      (with-temp-file (expand-file-name "pyproject.toml" project-dir)
        (insert "[project]\nname = \"test\"\n"))
      (with-temp-file (expand-file-name "package.json" project-dir)
        (insert "{}"))
      (let ((major-mode 'python-mode))
        (should (eglot-python-preset--project-find src-dir)))
      (let ((major-mode 'jtsx-typescript-mode))
        (should-not (eglot-python-preset--project-find src-dir))))))

(ert-deftest eglot-python-preset-project-root-returns-correct-dir ()
  "Project root returns the correct directory."
  (my-test-with-tmp-dir tmp-dir
    (let* ((project-dir (expand-file-name "myproject/" tmp-dir))
           (src-file (expand-file-name "src/main.py" project-dir)))
      (make-directory (file-name-directory src-file) t)
      (with-temp-file (expand-file-name "pyproject.toml" project-dir)
        (insert "[project]\nname = \"test\"\n"))
      (with-temp-file src-file (insert ""))
      (with-current-buffer (find-file-noselect src-file)
        (unwind-protect
            (should (string= (eglot-python-preset--project-root)
                             (file-name-as-directory project-dir)))
          (kill-buffer))))))

(ert-deftest eglot-python-preset-monorepo-project-boundary ()
  "In a monorepo, project-find-file escapes the LSP project boundary.
With eglot-lsp-context, --project-find scopes to the Python project.
Without it, project-try-vc returns the git root, and project-files
respects .gitignore (excludes dist/) while including files outside
the Python project boundary."
  (my-test-with-tmp-dir tmp-dir
    (let* ((monorepo-dir (my-test-copy-fixture-dir "monorepo" tmp-dir))
           (frontend-dir (expand-file-name "frontend/" monorepo-dir))
           (frontend-src (expand-file-name "src/" frontend-dir))
           (frontend-dist (expand-file-name "dist/" frontend-dir))
           (default-directory monorepo-dir))
      ;; Initialize git repo and commit the fixture files
      (call-process "git" nil nil nil "init" "-q" monorepo-dir)
      (call-process "git" nil nil nil "-C" monorepo-dir "add" ".")
      (call-process "git" nil nil nil "-C" monorepo-dir
                    "-c" "user.name=Test" "-c" "user.email=test@test"
                    "commit" "-q" "-m" "init")
      ;; Create dist/ file after commit (gitignored build output)
      (make-directory frontend-dist t)
      (with-temp-file (expand-file-name "output.py" frontend-dist)
        (insert "compiled output\n"))
      ;; 1. With eglot-lsp-context, project-find scopes to frontend
      (let ((eglot-lsp-context t)
            (major-mode 'python-mode))
        (let ((result (eglot-python-preset--project-find frontend-src)))
          (should result)
          (should (eq (car result) 'python-project))
          (should (string= (cdr result)
                           (file-name-as-directory frontend-dir)))))
      ;; 2. Without eglot-lsp-context, project-find returns nil
      (let ((eglot-lsp-context nil))
        (should-not (eglot-python-preset--project-find frontend-src)))
      ;; 3. project-try-vc finds the git root (monorepo root)
      (let ((vc-project (project-try-vc frontend-src)))
        (should vc-project)
        (should (string= (file-name-as-directory monorepo-dir)
                          (project-root vc-project)))
        ;; 4. project-files includes backend but excludes dist/
        (let ((files (project-files vc-project)))
          (should (cl-some (lambda (f) (string-suffix-p "backend/app.py" f))
                           files))
          (should (cl-some (lambda (f) (string-suffix-p "frontend/src/main.py" f))
                           files))
          (should-not (cl-some (lambda (f) (string-match-p "dist/" f))
                               files)))))))

(ert-deftest eglot-python-preset-setup-registers-hooks-and-contact ()
  (let ((eglot-server-programs nil)
        (project-find-functions nil)
        (python-base-mode-hook nil)
        (eglot-python-preset--setup-done nil))
    (cl-letf (((symbol-function 'eglot--workspace-configuration-plist)
               (lambda (&rest _) nil))
              ((symbol-function 'eglot-client-capabilities)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (eglot-python-preset-setup)
            (should (equal `(,eglot-python-preset-python-modes .
                             eglot-python-preset--server-contact)
                           (car eglot-server-programs)))
            (should (memq #'eglot-python-preset--project-find project-find-functions))
            (should (memq #'eglot-ensure python-base-mode-hook))
            (should (advice-member-p
                     #'eglot-python-preset--workspace-configuration-plist-a
                     'eglot--workspace-configuration-plist))
            (should (advice-member-p
                     #'eglot-python-preset--client-capabilities-a
                     'eglot-client-capabilities)))
        (advice-remove 'eglot--workspace-configuration-plist
                       #'eglot-python-preset--workspace-configuration-plist-a)
        (advice-remove 'eglot-client-capabilities
                       #'eglot-python-preset--client-capabilities-a)))))

(ert-deftest eglot-python-preset-maybe-setup-runs-when-auto-setup-t ()
  "Auto-setup calls setup when `eglot-python-preset-auto-setup' is t."
  (let ((eglot-server-programs nil)
        (project-find-functions nil)
        (python-base-mode-hook nil)
        (eglot-python-preset--setup-done nil)
        (eglot-python-preset-auto-setup t))
    (cl-letf (((symbol-function 'eglot--workspace-configuration-plist)
               (lambda (&rest _) nil))
              ((symbol-function 'eglot-client-capabilities)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (eglot-python-preset--maybe-setup)
            (should (equal `(,eglot-python-preset-python-modes .
                             eglot-python-preset--server-contact)
                           (car eglot-server-programs)))
            (should (memq #'eglot-python-preset--project-find project-find-functions)))
        (advice-remove 'eglot--workspace-configuration-plist
                       #'eglot-python-preset--workspace-configuration-plist-a)
        (advice-remove 'eglot-client-capabilities
                       #'eglot-python-preset--client-capabilities-a)))))

(ert-deftest eglot-python-preset-maybe-setup-skips-when-auto-setup-nil ()
  "Auto-setup skips setup when `eglot-python-preset-auto-setup' is nil."
  (let ((eglot-server-programs nil)
        (project-find-functions nil)
        (python-base-mode-hook nil)
        (eglot-python-preset--setup-done nil)
        (eglot-python-preset-auto-setup nil))
    (eglot-python-preset--maybe-setup)
    (should (null eglot-server-programs))
    (should (null project-find-functions))
    (should (null python-base-mode-hook))))

(ert-deftest eglot-python-preset-client-capabilities-injects-streaming ()
  "Capabilities advice injects $streamingDiagnostics for Python modes."
  (let ((base-caps '(:textDocument (:publishDiagnostics (:relatedInformation t))
                     :workspace (:configuration t))))
    (cl-letf (((symbol-function 'eglot--major-modes)
               (lambda (_s) '(python-ts-mode))))
      (let ((result (eglot-python-preset--client-capabilities-a
                     (lambda (_s) base-caps)
                     'mock-server)))
        (should (eq t (plist-get (plist-get result :textDocument)
                                 :$streamingDiagnostics)))))))

(ert-deftest eglot-python-preset-client-capabilities-skips-non-python-modes ()
  "Capabilities advice does not inject $streamingDiagnostics for other modes."
  (let ((base-caps '(:textDocument (:publishDiagnostics (:relatedInformation t))
                     :workspace (:configuration t))))
    (cl-letf (((symbol-function 'eglot--major-modes)
               (lambda (_s) '(typescript-ts-mode))))
      (let ((result (eglot-python-preset--client-capabilities-a
                     (lambda (_s) base-caps)
                     'mock-server)))
        (should-not (plist-get (plist-get result :textDocument)
                               :$streamingDiagnostics))))))

(defun my-test--streaming-merge (uri)
  "Compute merged diagnostics vector for URI from the streaming table."
  (let ((by-token (gethash uri eglot-python-preset--streaming-diag-table))
        (merged []))
    (when by-token
      (maphash (lambda (_tok diags) (setq merged (vconcat merged diags))) by-token))
    merged))

(ert-deftest eglot-python-preset-streaming-diags-accumulates-across-tokens ()
  "Streaming handler merges diagnostics from different tokens."
  (clrhash eglot-python-preset--streaming-diag-table)
  (let ((uri "file:///test-accum.py")
        (diag1 [:message "err1"])
        (diag2 [:message "err2"]))
    (let ((by-token (puthash uri (make-hash-table :test #'equal)
                             eglot-python-preset--streaming-diag-table)))
      (puthash "server-a" (vector diag1) by-token)
      (should (= 1 (length (my-test--streaming-merge uri))))
      (puthash "server-b" (vector diag2) by-token)
      (let ((merged (my-test--streaming-merge uri)))
        (should (vectorp merged))
        (should (= 2 (length merged)))))))

(ert-deftest eglot-python-preset-streaming-diags-replaces-same-token ()
  "Same token replaces its diagnostics, not accumulates."
  (clrhash eglot-python-preset--streaming-diag-table)
  (let ((uri "file:///test-replace.py")
        (diag1 [:message "err1"]))
    (let ((by-token (puthash uri (make-hash-table :test #'equal)
                             eglot-python-preset--streaming-diag-table)))
      (puthash "server-a" (vector diag1) by-token)
      (should (= 1 (length (my-test--streaming-merge uri))))
      (puthash "server-a" [] by-token)
      (should (= 0 (length (my-test--streaming-merge uri)))))))

(ert-deftest eglot-python-preset-streaming-diags-cleared-on-connect ()
  "Capabilities advice clears the streaming diagnostics table."
  (clrhash eglot-python-preset--streaming-diag-table)
  (puthash "file:///old.py" (make-hash-table :test #'equal)
           eglot-python-preset--streaming-diag-table)
  (should (= 1 (hash-table-count eglot-python-preset--streaming-diag-table)))
  (let ((base-caps '(:textDocument (:publishDiagnostics (:relatedInformation t)))))
    (cl-letf (((symbol-function 'eglot--major-modes)
               (lambda (_s) '(python-ts-mode))))
      (eglot-python-preset--client-capabilities-a
       (lambda (_s) base-caps) 'mock-server)))
  (should (= 0 (hash-table-count eglot-python-preset--streaming-diag-table))))

(ert-deftest eglot-python-preset-rass-live-ruff-symbol-uses-default-command ()
  (skip-unless (my-test-live-tests-enabled-p))
  (skip-unless (executable-find "python3"))
  (skip-unless (executable-find "rass"))
  (let ((result (my-test-run-live-fake-ruff-client '(ruff))))
    (my-test-assert-diagnostics result '("FLAG_MISSING") '("fake-ruff"))))

(ert-deftest eglot-python-preset-rass-live-ruff-isolated-passthrough ()
  (skip-unless (my-test-live-tests-enabled-p))
  (skip-unless (executable-find "python3"))
  (skip-unless (executable-find "rass"))
  (let ((result (my-test-run-live-fake-ruff-client '(["ruff" "--isolated" "server"]))))
    (my-test-assert-diagnostics result '("FLAG_PASSED") '("fake-ruff"))))

(ert-deftest eglot-python-preset-rass-live-ty-ruff-default-init-options ()
  (skip-unless (my-test-live-tests-enabled-p))
  (skip-unless (executable-find "python3"))
  (skip-unless (executable-find "rass"))
  (let ((result (my-test-run-live-fake-ty-ruff-client '(ty ruff))))
    (my-test-assert-diagnostics
     result '("FLAG_MISSING" "INIT_OPTIONS_PRESENT") '("fake-ruff"))))

(ert-deftest eglot-python-preset-rass-live-real-ruff-smoke ()
  (skip-unless (my-test-live-tests-enabled-p))
  (skip-unless (executable-find "python3"))
  (skip-unless (executable-find "rass"))
  (skip-unless (executable-find "ruff"))
  (my-test-with-tmp-dir tmp-dir
    (my-test--setup-fixture-dir "ruff" tmp-dir)
    (let* ((unused (expand-file-name "unused-import.py" tmp-dir))
           (valid (expand-file-name "valid.py" tmp-dir))
           (preset-path (my-test-rass-preset-path unused '(ruff)))
           (result (my-test--run-rass-session
                    preset-path
                    `((,unused . "python")
                      (,valid . "python"))
                    tmp-dir)))
      (my-test--assert-file-diagnostics result unused '("F401") '("ruff"))
      (my-test--assert-file-diagnostics result valid '()))))

(ert-deftest eglot-python-preset-rass-live-real-ty-smoke ()
  (skip-unless (my-test-live-tests-enabled-p))
  (skip-unless (executable-find "python3"))
  (skip-unless (executable-find "rass"))
  (skip-unless (executable-find "ty"))
  (skip-unless (executable-find "uv"))
  (my-test-with-tmp-dir tmp-dir
    (my-test--setup-fixture-dir "ty" tmp-dir)
    (let* ((unresolved (expand-file-name "pep-unresolved-import.py" tmp-dir))
           (valid (expand-file-name "pep-valid.py" tmp-dir))
           (default-directory tmp-dir))
      (unless (zerop (call-process "uv" nil nil nil
                                   "sync" "--script" unresolved))
        (error "uv sync --script failed for %s" unresolved))
      (unless (zerop (call-process "uv" nil nil nil
                                   "sync" "--script" valid))
        (error "uv sync --script failed for %s" valid))
      (let* ((preset-path (my-test-rass-preset-path unresolved '(ty)))
             (result (my-test--run-rass-session
                      preset-path
                      `((,unresolved . "python")
                        (,valid . "python"))
                      tmp-dir)))
        (should (member "ty"
                        (append (alist-get 'workspaceConfigSections result) nil)))
        (my-test--assert-file-diagnostics
         result unresolved '("unresolved-import") '("ty"))
        (my-test--assert-file-diagnostics result valid '())))))

(ert-deftest eglot-python-preset-rass-live-real-ty-ruff-smoke ()
  (skip-unless (my-test-live-tests-enabled-p))
  (skip-unless (executable-find "python3"))
  (skip-unless (executable-find "rass"))
  (skip-unless (executable-find "ty"))
  (skip-unless (executable-find "ruff"))
  (skip-unless (executable-find "uv"))
  (my-test-with-tmp-dir tmp-dir
    (my-test--setup-fixture-dir "ty" tmp-dir)
    (let* ((unresolved (expand-file-name "pep-unresolved-import.py" tmp-dir))
           (valid (expand-file-name "pep-valid.py" tmp-dir))
           (default-directory tmp-dir))
      (unless (zerop (call-process "uv" nil nil nil
                                   "sync" "--script" unresolved))
        (error "uv sync --script failed for %s" unresolved))
      (unless (zerop (call-process "uv" nil nil nil
                                   "sync" "--script" valid))
        (error "uv sync --script failed for %s" valid))
      (let* ((preset-path (my-test-rass-preset-path unresolved '(ty ruff)))
             (result (my-test--run-rass-session
                      preset-path
                      `((,unresolved . "python")
                        (,valid . "python"))
                      tmp-dir)))
        (should (member "ty"
                        (append (alist-get 'workspaceConfigSections result) nil)))
        (my-test--assert-file-diagnostics
         result unresolved '("F401" "unresolved-import") '("ty" "ruff"))
        (my-test--assert-file-diagnostics result valid '())))))

(ert-deftest eglot-python-preset-rass-live-real-basedpyright-smoke ()
  (skip-unless (my-test-live-tests-enabled-p))
  (skip-unless (executable-find "python3"))
  (skip-unless (executable-find "rass"))
  (skip-unless (executable-find "basedpyright-langserver"))
  (skip-unless (executable-find "uv"))
  (my-test-with-tmp-dir tmp-dir
    (my-test--setup-fixture-dir "basedpyright" tmp-dir)
    (let* ((unresolved (expand-file-name "pep-unresolved-import.py" tmp-dir))
           (valid (expand-file-name "pep-valid.py" tmp-dir))
           (default-directory tmp-dir))
      (unless (zerop (call-process "uv" nil nil nil
                                   "sync" "--script" unresolved))
        (error "uv sync --script failed for %s" unresolved))
      (unless (zerop (call-process "uv" nil nil nil
                                   "sync" "--script" valid))
        (error "uv sync --script failed for %s" valid))
      (let* ((preset-path (my-test-rass-preset-path unresolved '(basedpyright)))
             (result (my-test--run-rass-session
                      preset-path
                      `((,unresolved . "python")
                        (,valid . "python"))
                      tmp-dir)))
        (should (cl-intersection
                 (append (alist-get 'workspaceConfigSections result) nil)
                 '("python" "basedpyright" "basedpyright.analysis")
                 :test #'equal))
        (my-test--assert-file-diagnostics
         result unresolved
         '("reportMissingModuleSource" "reportUnusedImport") '("basedpyright"))
        (my-test--assert-file-diagnostics result valid '())))))

(ert-deftest eglot-python-preset-rass-live-real-ty-non-pep-smoke ()
  (skip-unless (my-test-live-tests-enabled-p))
  (skip-unless (executable-find "python3"))
  (skip-unless (executable-find "rass"))
  (skip-unless (executable-find "ty"))
  (my-test-with-tmp-dir tmp-dir
    (my-test--setup-fixture-dir "ty" tmp-dir)
    (let* ((unresolved (expand-file-name "unresolved-import.py" tmp-dir))
           (valid (expand-file-name "valid.py" tmp-dir))
           (preset-path (my-test-rass-preset-path unresolved '(ty)))
           (result (my-test--run-rass-session
                    preset-path
                    `((,unresolved . "python")
                      (,valid . "python"))
                    tmp-dir)))
      (should (member "ty"
                      (append (alist-get 'workspaceConfigSections result) nil)))
      (my-test--assert-file-diagnostics
       result unresolved '("unresolved-import") '("ty"))
      (my-test--assert-file-diagnostics result valid '()))))

(ert-deftest eglot-python-preset-rass-live-real-basedpyright-non-pep-smoke ()
  (skip-unless (my-test-live-tests-enabled-p))
  (skip-unless (executable-find "python3"))
  (skip-unless (executable-find "rass"))
  (skip-unless (executable-find "basedpyright-langserver"))
  (my-test-with-tmp-dir tmp-dir
    (my-test--setup-fixture-dir "basedpyright" tmp-dir)
    (let* ((unresolved (expand-file-name "unresolved-import.py" tmp-dir))
           (valid (expand-file-name "valid.py" tmp-dir))
           (preset-path (my-test-rass-preset-path unresolved '(basedpyright)))
           (result (my-test--run-rass-session
                    preset-path
                    `((,unresolved . "python")
                      (,valid . "python"))
                    tmp-dir)))
      (should (cl-intersection
               (append (alist-get 'workspaceConfigSections result) nil)
               '("python" "basedpyright" "basedpyright.analysis")
               :test #'equal))
      (my-test--assert-file-diagnostics
       result unresolved
       '("reportMissingModuleSource" "reportUnusedImport") '("basedpyright"))
      (my-test--assert-file-diagnostics result valid '()))))

(ert-deftest eglot-python-preset-rass-live-real-basedpyright-ruff-smoke ()
  (skip-unless (my-test-live-tests-enabled-p))
  (skip-unless (executable-find "python3"))
  (skip-unless (executable-find "rass"))
  (skip-unless (executable-find "basedpyright-langserver"))
  (skip-unless (executable-find "ruff"))
  (skip-unless (executable-find "uv"))
  (my-test-with-tmp-dir tmp-dir
    (my-test--setup-fixture-dir "basedpyright-ruff" tmp-dir)
    (let* ((unresolved (expand-file-name "pep-unresolved-import.py" tmp-dir))
           (valid (expand-file-name "pep-valid.py" tmp-dir))
           (default-directory tmp-dir))
      (unless (zerop (call-process "uv" nil nil nil
                                   "sync" "--script" unresolved))
        (error "uv sync --script failed for %s" unresolved))
      (unless (zerop (call-process "uv" nil nil nil
                                   "sync" "--script" valid))
        (error "uv sync --script failed for %s" valid))
      (let* ((preset-path (my-test-rass-preset-path
                           unresolved '(basedpyright ruff)))
             (result (my-test--run-rass-session
                      preset-path
                      `((,unresolved . "python")
                        (,valid . "python"))
                      tmp-dir)))
        (should (cl-intersection
                 (append (alist-get 'workspaceConfigSections result) nil)
                 '("python" "basedpyright" "basedpyright.analysis")
                 :test #'equal))
        (my-test--assert-file-diagnostics
         result unresolved
         '("F401" "reportMissingModuleSource" "reportUnusedImport")
         '("basedpyright" "ruff"))
        (my-test--assert-file-diagnostics result valid '())))))

(ert-deftest eglot-python-preset-rass-live-real-basedpyright-ruff-non-pep-smoke ()
  (skip-unless (my-test-live-tests-enabled-p))
  (skip-unless (executable-find "python3"))
  (skip-unless (executable-find "rass"))
  (skip-unless (executable-find "basedpyright-langserver"))
  (skip-unless (executable-find "ruff"))
  (my-test-with-tmp-dir tmp-dir
    (my-test--setup-fixture-dir "basedpyright-ruff" tmp-dir)
    (let* ((unresolved (expand-file-name "unresolved-import.py" tmp-dir))
           (valid (expand-file-name "valid.py" tmp-dir))
           (preset-path (my-test-rass-preset-path
                         unresolved '(basedpyright ruff)))
           (result (my-test--run-rass-session
                    preset-path
                    `((,unresolved . "python")
                      (,valid . "python"))
                    tmp-dir)))
      (should (cl-intersection
               (append (alist-get 'workspaceConfigSections result) nil)
               '("python" "basedpyright" "basedpyright.analysis")
               :test #'equal))
      (my-test--assert-file-diagnostics
       result unresolved
       '("F401" "reportMissingModuleSource" "reportUnusedImport")
         '("basedpyright" "ruff"))
      (my-test--assert-file-diagnostics result valid '()))))

(ert-deftest eglot-python-preset-rass-live-real-ruff-ty-pep-smoke ()
  (skip-unless (my-test-live-tests-enabled-p))
  (skip-unless (executable-find "python3"))
  (skip-unless (executable-find "rass"))
  (skip-unless (executable-find "ty"))
  (skip-unless (executable-find "ruff"))
  (skip-unless (executable-find "uv"))
  (my-test-with-tmp-dir tmp-dir
    (my-test--setup-fixture-dir "ruff-ty" tmp-dir)
    (let* ((unresolved (expand-file-name "pep-unresolved-import.py" tmp-dir))
           (valid (expand-file-name "pep-valid.py" tmp-dir))
           (default-directory tmp-dir))
      (unless (zerop (call-process "uv" nil nil nil
                                   "sync" "--script" unresolved))
        (error "uv sync --script failed for %s" unresolved))
      (unless (zerop (call-process "uv" nil nil nil
                                   "sync" "--script" valid))
        (error "uv sync --script failed for %s" valid))
      (let* ((preset-path (my-test-rass-preset-path unresolved '(ty ruff)))
             (result (my-test--run-rass-session
                      preset-path
                      `((,unresolved . "python")
                        (,valid . "python"))
                      tmp-dir)))
        (should (member "ty"
                        (append (alist-get 'workspaceConfigSections result) nil)))
        (my-test--assert-file-diagnostics
         result unresolved '("F401" "unresolved-import") '("ty" "ruff"))
        (my-test--assert-file-diagnostics result valid '())))))

(ert-deftest eglot-python-preset-rass-live-real-ruff-ty-non-pep-smoke ()
  (skip-unless (my-test-live-tests-enabled-p))
  (skip-unless (executable-find "python3"))
  (skip-unless (executable-find "rass"))
  (skip-unless (executable-find "ty"))
  (skip-unless (executable-find "ruff"))
  (my-test-with-tmp-dir tmp-dir
    (my-test--setup-fixture-dir "ruff-ty" tmp-dir)
    (let* ((unresolved (expand-file-name "unresolved-import.py" tmp-dir))
           (valid (expand-file-name "valid.py" tmp-dir))
           (preset-path (my-test-rass-preset-path unresolved '(ty ruff)))
           (result (my-test--run-rass-session
                    preset-path
                    `((,unresolved . "python")
                      (,valid . "python"))
                    tmp-dir)))
      (should (member "ty"
                      (append (alist-get 'workspaceConfigSections result) nil)))
      (my-test--assert-file-diagnostics
       result unresolved '("F401" "unresolved-import") '("ty" "ruff"))
      (my-test--assert-file-diagnostics result valid '()))))

(defun my-test-run-live-tests-parallel ()
  "Run all live tests in parallel child Emacs processes, then exit.
Parallelism defaults to 6 or the LIVE_TEST_JOBS env variable."
  (let* ((max-jobs (string-to-number (or (getenv "LIVE_TEST_JOBS") "6")))
         (project-dir (expand-file-name ".." my-test-test-dir))
         (test-names
          (with-temp-buffer
            (insert-file-contents
             (expand-file-name "test/test.el" project-dir))
            (let (names)
              (while (re-search-forward
                      "ert-deftest \\(eglot-python-preset-rass-live-[^ ()]+\\)"
                      nil t)
                (push (match-string 1) names))
              (nreverse names))))
         (total (length test-names))
         (emacs-bin (expand-file-name invocation-name invocation-directory))
         (preset-el (expand-file-name
                     "eglot-python-preset.el" project-dir))
         (test-el (expand-file-name "test/test.el" project-dir))
         (running '())
         (passed 0)
         (failed 0)
         (failures '()))
    (message "Running %d live tests with up to %d parallel jobs..."
             total max-jobs)
    (while (or test-names running)
      ;; Launch jobs up to max-jobs
      (while (and test-names (< (length running) max-jobs))
        (let* ((name (pop test-names))
               (buf (generate-new-buffer (concat " *live-test:" name "*")))
               (proc (start-process
                      name buf emacs-bin
                      "-Q" "--batch"
                      "-l" preset-el
                      "--eval" "(setq my-test-run-live-tests t)"
                      "-l" test-el
                      "--eval"
                      (format "(ert-run-tests-batch-and-exit \"^%s$\")" name))))
          (set-process-sentinel proc #'ignore)
          (push (list name proc buf) running)))
      ;; Poll for completion
      (sleep-for 0.1)
      (let (still-running)
        (dolist (entry running)
          (cl-destructuring-bind (name proc buf) entry
            (if (process-live-p proc)
                (push entry still-running)
              (let ((rc (process-exit-status proc))
                    (output (with-current-buffer buf (buffer-string))))
                (if (= rc 0)
                    (progn
                      (cl-incf passed)
                      (message "PASS: %s" name))
                  (cl-incf failed)
                  (push name failures)
                  (message "FAIL: %s" name)
                  (message "%s" (car (last (split-string output "\n\n")))))
                (kill-buffer buf)))))
        (setq running (nreverse still-running))))
    (message "\n%d/%d live tests passed." passed total)
    (when failures
      (message "Failures: %s" (string-join (nreverse failures) ", ")))
    (kill-emacs (if (= failed 0) 0 1))))

;;; test/test.el ends here
