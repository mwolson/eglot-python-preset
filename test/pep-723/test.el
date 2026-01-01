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

  (princ "Test 6a: Workspace config advice with directory path (basedpyright)... ")
  (let ((eglot-python-preset-lsp-server 'basedpyright))
    (with-current-buffer (find-file-noselect my-pep723-test-script-1)
      ;; Test with directory path (simulates workspace/configuration request with scopeUri)
      ;; Uses managed buffer since path is a directory
      (let* ((mock-server (list :managed-buffers (list (current-buffer))))
             (config (cl-letf (((symbol-function 'eglot--managed-buffers)
                                (lambda (_s) (plist-get mock-server :managed-buffers))))
                       (eglot-python-preset--workspace-configuration-plist-a
                        (lambda (_server &optional _path) nil)
                        mock-server
                        my-pep723-test-dir))))
        (if (plist-get config :python)
            (princ "PASS\n")
          (princ "FAIL\n")
          (kill-emacs 1)))))

  (princ "Test 6b: Workspace config advice without path arg (basedpyright)... ")
  (let ((eglot-python-preset-lsp-server 'basedpyright))
    (with-current-buffer (find-file-noselect my-pep723-test-script-1)
      ;; Test without path arg (simulates didChangeConfiguration)
      ;; Mock server with this buffer as managed buffer
      (let* ((mock-server (list :managed-buffers (list (current-buffer))))
             (config (cl-letf (((symbol-function 'eglot--managed-buffers)
                                (lambda (_s) (plist-get mock-server :managed-buffers))))
                       (eglot-python-preset--workspace-configuration-plist-a
                        (lambda (_server &optional _path) nil)
                        mock-server
                        nil))))
        (if (plist-get config :python)
            (princ "PASS\n")
          (princ "FAIL\n")
          (kill-emacs 1)))))

  (princ "Test 6c: Advice with no managed buffers returns base config... ")
  (let ((eglot-python-preset-lsp-server 'basedpyright)
        (base-config '(:some (:setting "value"))))
    ;; Mock server with no managed buffers
    (let* ((mock-server (list :managed-buffers nil))
           (config (cl-letf (((symbol-function 'eglot--managed-buffers)
                              (lambda (_s) (plist-get mock-server :managed-buffers))))
                     (eglot-python-preset--workspace-configuration-plist-a
                      (lambda (_server &optional _path) base-config)
                      mock-server
                      my-pep723-test-dir))))
      (if (and (equal config base-config)
               (not (plist-get config :python)))
          (princ "PASS\n")
        (princ (format "FAIL (got %S)\n" config))
        (kill-emacs 1))))

  (princ "Test 7: Non-PEP-723 file returns base config unchanged... ")
  (let ((eglot-python-preset-lsp-server 'basedpyright)
        (regular-py (make-temp-file "test" nil ".py" "print('hello')\n")))
    (unwind-protect
        (with-current-buffer (find-file-noselect regular-py)
          (let* ((base-config '(:basedpyright (:setting "value")))
                 (mock-server (list :managed-buffers (list (current-buffer))))
                 (config (cl-letf (((symbol-function 'eglot--managed-buffers)
                                    (lambda (_s) (plist-get mock-server :managed-buffers))))
                           (eglot-python-preset--workspace-configuration-plist-a
                            (lambda (_server &optional _path) base-config)
                            mock-server
                            "/tmp"))))
            (if (and (equal config base-config)
                     (not (plist-get config :python)))
                (princ "PASS\n")
              (princ (format "FAIL (got %S)\n" config))
              (kill-emacs 1))))
      (delete-file regular-py)))

  (princ "Test 8: Non-basedpyright server returns base config unchanged... ")
  (let ((eglot-python-preset-lsp-server 'ty))
    (with-current-buffer (find-file-noselect my-pep723-test-script-1)
      (let* ((base-config '(:some (:config "value")))
             (mock-server (list :managed-buffers (list (current-buffer))))
             (config (cl-letf (((symbol-function 'eglot--managed-buffers)
                                (lambda (_s) (plist-get mock-server :managed-buffers))))
                       (eglot-python-preset--workspace-configuration-plist-a
                        (lambda (_server &optional _path) base-config)
                        mock-server
                        my-pep723-test-dir))))
        (if (equal config base-config)
            (princ "PASS\n")
          (princ (format "FAIL (got %S)\n" config))
          (kill-emacs 1)))))

  (princ "Test 8b: PEP-723 script merges with user's eglot-workspace-configuration... ")
  (let ((eglot-python-preset-lsp-server 'basedpyright)
        (user-config '(:basedpyright.analysis (:typeCheckingMode "strict"))))
    (with-current-buffer (find-file-noselect my-pep723-test-script-1)
      (let* ((mock-server (list :managed-buffers (list (current-buffer))))
             (config (cl-letf (((symbol-function 'eglot--managed-buffers)
                                (lambda (_s) (plist-get mock-server :managed-buffers))))
                       (eglot-python-preset--workspace-configuration-plist-a
                        (lambda (_server &optional _path) user-config)
                        mock-server
                        my-pep723-test-dir))))
        (if (and (plist-get config :python)
                 (plist-get (plist-get config :python) :pythonPath)
                 (plist-get config :basedpyright.analysis)
                 (equal (plist-get (plist-get config :basedpyright.analysis)
                                   :typeCheckingMode)
                        "strict"))
            (princ "PASS\n")
          (princ (format "FAIL (got %S)\n" config))
          (kill-emacs 1)))))

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
