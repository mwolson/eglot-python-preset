;;; test/pep-723/test.el --- Test PEP-723 support for eglot-python-preset library -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path (expand-file-name "../../elisp" (file-name-directory load-file-name)))

(require 'eglot-python-preset)

;; Stub eglot variables for testing without loading full eglot
(defvar eglot-server-programs nil)
(defvar eglot-workspace-configuration nil)

(defvar my-pep723-test-dir (file-name-directory load-file-name))
(defvar my-pep723-test-script (expand-file-name "example.py" my-pep723-test-dir))

(defun my-pep723-run-tests ()
  "Run PEP-723 tests and report results."

  (princ "Test 1: Detection (with metadata)... ")
  (if (eglot-python-preset-has-metadata-p my-pep723-test-script)
      (princ "PASS\n")
    (princ "FAIL\n")
    (kill-emacs 1))

  (princ "Test 2: Detection (without metadata)... ")
  (let ((regular-py (make-temp-file "test" nil ".py" "print('hello')\n")))
    (unwind-protect
        (if (not (eglot-python-preset-has-metadata-p regular-py))
            (princ "PASS\n")
          (princ "FAIL\n")
          (kill-emacs 1))
      (delete-file regular-py)))

  (princ "Test 3: Detection (incomplete block)... ")
  (let ((incomplete-py (make-temp-file "test" nil ".py" "# /// script\n# no closing\n")))
    (unwind-protect
        (if (not (eglot-python-preset-has-metadata-p incomplete-py))
            (princ "PASS\n")
          (princ "FAIL\n")
          (kill-emacs 1))
      (delete-file incomplete-py)))

  (princ "Test 4: uv python find... ")
  (if (not (executable-find "uv"))
      (princ "SKIP (uv not found)\n")
    (let ((python-path (eglot-python-preset-get-python-path my-pep723-test-script)))
      (cond
       ((null python-path)
        (princ "WARN (no path returned)\n"))
       ((file-exists-p python-path)
        (princ (format "PASS (%s)\n" python-path)))
       (t
        (princ (format "WARN (path doesn't exist: %s)\n" python-path))))))

  (princ "Test 5: Init options (ty)... ")
  (let ((eglot-python-preset-lsp-server 'ty))
    (with-temp-buffer
      (setq buffer-file-name my-pep723-test-script)
      (let ((opts (eglot-python-preset--init-options)))
        (if (plist-get opts :configuration)
            (princ "PASS\n")
          (princ "FAIL\n")
          (kill-emacs 1)))))

  (princ "Test 6: Workspace config hash table (basedpyright)... ")
  (let ((eglot-python-preset-lsp-server 'basedpyright))
    (with-temp-buffer
      (setq buffer-file-name my-pep723-test-script)
      (eglot-python-preset--setup-buffer)
      ;; Config is stored in hash table keyed by directory
      (let* ((script-dir (file-name-as-directory
                          (expand-file-name (file-name-directory my-pep723-test-script))))
             (config (gethash script-dir eglot-python-preset--workspace-configs)))
        (if (plist-get config :python)
            (princ "PASS\n")
          (princ "FAIL\n")
          (kill-emacs 1)))))

  (princ "Test 7: Workspace config lookup via function (basedpyright)... ")
  (let ((eglot-python-preset-lsp-server 'basedpyright))
    ;; First, register the config by simulating opening the file
    (with-temp-buffer
      (setq buffer-file-name my-pep723-test-script)
      (eglot-python-preset--setup-buffer))
    ;; Now call eglot-python-preset-setup to install our function as default
    (eglot-python-preset-setup)
    ;; Simulate what Eglot does: create a temp buffer, set default-directory,
    ;; and call eglot-workspace-configuration
    (let* ((script-dir (file-name-as-directory
                        (expand-file-name (file-name-directory my-pep723-test-script))))
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

  (princ "\nAll tests passed.\n"))

(my-pep723-run-tests)
