;; package-lint-local.el --- -*- lexical-binding: t; -*-

(require 'package)

(let* ((topdir (expand-file-name (or (car argv) default-directory)))
       (file (expand-file-name (or (cadr argv) "eglot-python-preset.el")
                               topdir)))
  (setq package-user-dir (expand-file-name "tmp/package-lint-elpa" topdir))
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (unless (package-installed-p 'package-lint)
    (package-install 'package-lint))
  (require 'package-lint)
  (find-file file)
  (let* ((allowed-extern '("eglot-lsp-context"))
         (issues
          (seq-remove
           (lambda (issue)
             (pcase-let ((`(,_line ,_column ,_level ,message) issue))
               (seq-some (lambda (sym)
                           (string-match-p
                            (regexp-quote (format "\"%s\"" sym))
                            message))
                         allowed-extern)))
           (package-lint-buffer))))
    (if issues
        (progn
          (dolist (issue issues)
            (pcase-let ((`(,line ,column ,level ,message) issue))
              (princ (format "%s:%s:%s: %s: %s\n"
                             file line column level message))))
          (kill-emacs 1))
      (princ "ok\n"))))

(provide 'package-lint-local)
;;; package-lint-local.el ends here
