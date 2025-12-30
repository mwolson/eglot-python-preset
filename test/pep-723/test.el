;;; test/pep-723/test.el --- Test PEP-723 support for eglot-python-preset library -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path (expand-file-name "../../elisp" (file-name-directory load-file-name)))

(require 'eglot-python-preset)

;; Stub eglot variables for testing without loading full eglot
(defvar eglot-server-programs nil)
(defvar eglot-workspace-configuration nil)

(defvar my-pep723-test-dir (file-name-directory load-file-name))
(defvar my-pep723-test-script-1 (expand-file-name "example-1.py" my-pep723-test-dir))
(defvar my-pep723-test-script-2 (expand-file-name "example-2.py" my-pep723-test-dir))

(defun my-pep723-run-tests ()
  "Run PEP-723 tests and report results."

  (princ "Test 1a: Detection (with metadata, example-1)... ")
  (with-current-buffer (find-file-noselect my-pep723-test-script-1)
    (if (eglot-python-preset-has-metadata-p)
        (princ "PASS\n")
      (princ "FAIL\n")
      (kill-emacs 1)))

  (princ "Test 1b: Detection (with metadata, example-2 with docstring/CRLF)... ")
  (with-current-buffer (find-file-noselect my-pep723-test-script-2)
    (if (eglot-python-preset-has-metadata-p)
        (princ "PASS\n")
      (princ "FAIL\n")
      (kill-emacs 1)))

  (princ "Test 2: Detection (without metadata)... ")
  (let ((regular-py (make-temp-file "test" nil ".py" "print('hello')\n")))
    (unwind-protect
        (with-current-buffer (find-file-noselect regular-py)
          (if (not (eglot-python-preset-has-metadata-p))
              (princ "PASS\n")
            (princ "FAIL\n")
            (kill-emacs 1)))
      (delete-file regular-py)))

  (princ "Test 3: Detection (incomplete block)... ")
  (let ((incomplete-py (make-temp-file "test" nil ".py" "# /// script\n# no closing\n")))
    (unwind-protect
        (with-current-buffer (find-file-noselect incomplete-py)
          (if (not (eglot-python-preset-has-metadata-p))
              (princ "PASS\n")
            (princ "FAIL\n")
            (kill-emacs 1)))
      (delete-file incomplete-py)))

  (princ "Test 4: uv python find... ")
  (if (not (executable-find "uv"))
      (princ "SKIP (uv not found)\n")
    (let ((python-path (eglot-python-preset-get-python-path my-pep723-test-script-1)))
      (cond
       ((null python-path)
        (princ "WARN (no path returned)\n"))
       ((file-exists-p python-path)
        (princ (format "PASS (%s)\n" python-path)))
       (t
        (princ (format "WARN (path doesn't exist: %s)\n" python-path))))))

  (princ "Test 5: Init options (ty)... ")
  (let ((eglot-python-preset-lsp-server 'ty))
    (with-current-buffer (find-file-noselect my-pep723-test-script-1)
      (let ((opts (eglot-python-preset--init-options)))
        (if (plist-get opts :configuration)
            (princ "PASS\n")
          (princ "FAIL\n")
          (kill-emacs 1)))))

  (princ "Test 6: Workspace config hash table (basedpyright)... ")
  (let ((eglot-python-preset-lsp-server 'basedpyright))
    (with-current-buffer (find-file-noselect my-pep723-test-script-1)
      (eglot-python-preset--setup-buffer)
      ;; Config is stored in hash table keyed by directory
      (let* ((script-dir (file-name-as-directory
                          (expand-file-name (file-name-directory my-pep723-test-script-1))))
             (config (gethash script-dir eglot-python-preset--workspace-configs)))
        (if (plist-get config :python)
            (princ "PASS\n")
          (princ "FAIL\n")
          (kill-emacs 1)))))

  (princ "Test 7: Workspace config lookup via function (basedpyright)... ")
  (let ((eglot-python-preset-lsp-server 'basedpyright))
    ;; First, register the config by simulating opening the file
    (with-current-buffer (find-file-noselect my-pep723-test-script-1)
      (eglot-python-preset--setup-buffer))
    ;; Now call eglot-python-preset-setup to install our function as default
    (eglot-python-preset-setup)
    ;; Simulate what Eglot does: create a temp buffer, set default-directory,
    ;; and call eglot-workspace-configuration
    (let* ((script-dir (file-name-as-directory
                        (expand-file-name (file-name-directory my-pep723-test-script-1))))
           (config (with-temp-buffer
                     (setq default-directory script-dir)
                     (if (functionp eglot-workspace-configuration)
                         (funcall eglot-workspace-configuration nil)
                       eglot-workspace-configuration))))
      (if (plist-get config :python)
          (princ "PASS\n")
        (princ (format "FAIL (got %S)\n" config))
        (kill-emacs 1))))

  (princ "Test 8: Non-PEP-723 dir returns nil (basedpyright)... ")
  (let ((eglot-python-preset-lsp-server 'basedpyright))
    ;; Simulate Eglot looking up config for a directory with no PEP-723 script
    (let* ((other-dir (file-name-as-directory (expand-file-name "/tmp")))
           (config (with-temp-buffer
                     (setq default-directory other-dir)
                     (if (functionp eglot-workspace-configuration)
                         (funcall eglot-workspace-configuration nil)
                       eglot-workspace-configuration))))
      (if (null config)
          (princ "PASS\n")
        (princ (format "FAIL (expected nil, got %S)\n" config))
        (kill-emacs 1))))

  (princ "Test 9: Sync and remove environment... ")
  (if (not (executable-find "uv"))
      (princ "SKIP (uv not found)\n")
    (with-current-buffer (find-file-noselect my-pep723-test-script-1)
      ;; Remove any existing environment first to ensure clean state
      (let ((python-path (eglot-python-preset-get-python-path my-pep723-test-script-1)))
        (when python-path
          (let ((env-dir (eglot-python-preset--python-env-dir python-path)))
            (when (and env-dir (file-directory-p env-dir))
              (delete-directory env-dir t)))))
      ;; Now sync to create fresh environment
      (let* ((script-path (buffer-file-name))
             (default-directory (file-name-directory script-path))
             (status (call-process "uv" nil nil nil "sync" "--script" script-path)))
        (unless (zerop status)
          (princ (format "FAIL (sync failed with exit code %s)\n" status))
          (kill-emacs 1)))
      ;; Verify environment was created
      (let ((python-path (eglot-python-preset-get-python-path my-pep723-test-script-1)))
        (unless python-path
          (princ "FAIL (no python path after sync)\n")
          (kill-emacs 1))
        (let ((env-dir (eglot-python-preset--python-env-dir python-path)))
          (unless (and env-dir (file-directory-p env-dir))
            (princ "FAIL (environment directory not created)\n")
            (kill-emacs 1))
          ;; Now remove the environment
          (delete-directory env-dir t)
          (if (file-directory-p env-dir)
              (progn
                (princ "FAIL (environment not removed)\n")
                (kill-emacs 1))
            (princ "PASS\n"))))))

  (princ "\nAll tests passed.\n"))

(my-pep723-run-tests)
