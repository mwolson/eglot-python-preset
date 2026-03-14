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

(defun my-test-run-live-rass-client (preset-path python-file)
  "Run the Python live `rass` client for PRESET-PATH and PYTHON-FILE."
  (with-temp-buffer
    (let ((status (call-process "python3" nil (current-buffer) nil
                                my-test-live-rass-client
                                preset-path
                                python-file)))
      (unless (zerop status)
        (error "Live rass client failed: %s" (string-trim (buffer-string))))
      (goto-char (point-min))
      (json-parse-string (buffer-string)
                         :object-type 'alist
                         :array-type 'list
                         :null-object nil
                         :false-object nil))))

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
          (my-test-run-live-rass-client preset-path python-file))
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
          (my-test-run-live-rass-client preset-path python-file))
      (delete-directory project-dir t))))

(defun my-test-run-live-real-rass-client (tools mode)
  "Run live `rass` integration for TOOLS in MODE."
  (let ((project-dir (make-temp-file "rass-real" t)))
    (unwind-protect
        (pcase mode
          ('ruff
           (let* ((python-file (expand-file-name "main.py" project-dir))
                  (pyproject-file (expand-file-name "pyproject.toml" project-dir))
                  (preset-path nil))
             (with-temp-file pyproject-file
               (insert "[project]\n")
               (insert "name = \"ruff-smoke\"\n")
               (insert "version = \"0.0.0\"\n"))
             (with-temp-file python-file
               (insert "import os\n"))
             (setq preset-path (my-test-rass-preset-path python-file tools))
             (my-test-run-live-rass-client preset-path python-file)))
          ((or 'ty 'basedpyright)
           (let* ((python-file (expand-file-name "script.py" project-dir))
                  (preset-path nil)
                  (default-directory project-dir))
             (my-test-write-script-metadata-file python-file "import requests\n")
             (unless (zerop (call-process "uv" nil nil nil "sync" "--script" python-file))
               (error "uv sync --script failed for %s" python-file))
             (setq preset-path (my-test-rass-preset-path python-file tools))
             (my-test-run-live-rass-client preset-path python-file))))
      (delete-directory project-dir t))))

(defun my-test-normalized-diagnostic-sources (result)
  "Return RESULT diagnostic sources normalized to lowercase."
  (mapcar #'downcase (alist-get 'diagnosticSources result)))

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
                     "BASEDPYRIGHT_CONFIGURATION = None")))))
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
                     "TY_CONFIGURATION = None")))))
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
                     "TY_CONFIGURATION = None")))))
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
           (eglot-lsp-context t))
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
      (let ((eglot-lsp-context t))
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
        (python-base-mode-hook nil))
    (cl-letf (((symbol-function 'eglot--workspace-configuration-plist)
               (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (eglot-python-preset-setup)
            (should (equal '((python-ts-mode python-mode) .
                             eglot-python-preset--server-contact)
                           (car eglot-server-programs)))
            (should (memq #'eglot-python-preset--project-find project-find-functions))
            (should (memq #'eglot-ensure python-base-mode-hook))
            (should (advice-member-p
                     #'eglot-python-preset--workspace-configuration-plist-a
                     'eglot--workspace-configuration-plist)))
        (advice-remove 'eglot--workspace-configuration-plist
                       #'eglot-python-preset--workspace-configuration-plist-a)))))

(ert-deftest eglot-python-preset-rass-live-ruff-symbol-uses-default-command ()
  (skip-unless (my-test-live-tests-enabled-p))
  (skip-unless (executable-find "python3"))
  (skip-unless (executable-find "rass"))
  (let ((result (my-test-run-live-fake-ruff-client '(ruff))))
    (should (equal '("FLAG_MISSING")
                   (alist-get 'diagnosticCodes result)))))

(ert-deftest eglot-python-preset-rass-live-ruff-isolated-passthrough ()
  (skip-unless (my-test-live-tests-enabled-p))
  (skip-unless (executable-find "python3"))
  (skip-unless (executable-find "rass"))
  (let ((result (my-test-run-live-fake-ruff-client '(["ruff" "--isolated" "server"]))))
    (should (equal '("FLAG_PASSED")
                   (alist-get 'diagnosticCodes result)))))

(ert-deftest eglot-python-preset-rass-live-ty-ruff-default-init-options ()
  (skip-unless (my-test-live-tests-enabled-p))
  (skip-unless (executable-find "python3"))
  (skip-unless (executable-find "rass"))
  (let ((result (my-test-run-live-fake-ty-ruff-client '(ty ruff))))
    (should (member "INIT_OPTIONS_PRESENT"
                    (alist-get 'diagnosticCodes result)))))

(ert-deftest eglot-python-preset-rass-live-real-ruff-smoke ()
  (skip-unless (my-test-live-tests-enabled-p))
  (skip-unless (executable-find "python3"))
  (skip-unless (executable-find "rass"))
  (skip-unless (executable-find "ruff"))
  (let ((result (my-test-run-live-real-rass-client '(ruff) 'ruff)))
    (should (alist-get 'initialized result))
    (should (member "ruff" (my-test-normalized-diagnostic-sources result)))))

(ert-deftest eglot-python-preset-rass-live-real-ty-smoke ()
  (skip-unless (my-test-live-tests-enabled-p))
  (skip-unless (executable-find "python3"))
  (skip-unless (executable-find "rass"))
  (skip-unless (executable-find "ty"))
  (skip-unless (executable-find "uv"))
  (let ((result (my-test-run-live-real-rass-client '(ty) 'ty)))
    (should (alist-get 'initialized result))
    (should (member "ty" (alist-get 'workspaceConfigSections result)))))

(ert-deftest eglot-python-preset-rass-live-real-ty-ruff-smoke ()
  (skip-unless (my-test-live-tests-enabled-p))
  (skip-unless (executable-find "python3"))
  (skip-unless (executable-find "rass"))
  (skip-unless (executable-find "ty"))
  (skip-unless (executable-find "ruff"))
  (skip-unless (executable-find "uv"))
  (let ((result (my-test-run-live-real-rass-client '(ty ruff) 'ty)))
    (should (alist-get 'initialized result))
    (should (member "ty" (alist-get 'workspaceConfigSections result)))
    (should (cl-intersection (my-test-normalized-diagnostic-sources result)
                             '("ty" "ruff")
                             :test #'equal))))

(ert-deftest eglot-python-preset-rass-live-real-basedpyright-smoke ()
  (skip-unless (my-test-live-tests-enabled-p))
  (skip-unless (executable-find "python3"))
  (skip-unless (executable-find "rass"))
  (skip-unless (executable-find "basedpyright-langserver"))
  (skip-unless (executable-find "uv"))
  (let ((result (my-test-run-live-real-rass-client '(basedpyright) 'basedpyright))
        (sections nil))
    (setq sections (alist-get 'workspaceConfigSections result))
    (should (alist-get 'initialized result))
    (should (cl-intersection sections
                             '("python" "basedpyright" "basedpyright.analysis")
                             :test #'equal))))

;;; test/test.el ends here
