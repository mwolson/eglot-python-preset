;; byte-compile-local.el --- -*- lexical-binding: t -*-

(require 'treesit) ; to silence an autoload warning, seems like emacs bug
(require 'package)
(package-initialize)

(defun my-byte-compile-local-package (lib-name lib-path)
  (let* ((lib-sym (intern lib-name))
         (pkg (cadr (assq lib-sym (package--alist))))
         (dir-or-file (file-name-concat default-directory lib-path))
         (package-file (concat lib-name "-pkg.el")))
    (when pkg
      (package-delete pkg t))
    (cond ((string-match-p "\\.el\\'" dir-or-file)
           (message "Compiling %s..." (expand-file-name dir-or-file))
           (byte-compile-file dir-or-file))
          ((file-exists-p (expand-file-name package-file dir-or-file))
           (package-install-file dir-or-file))
          (t
           (require 'package-build)
           (let* ((package-build--inhibit-checkout t)
                  (package-build--inhibit-fetch 'strict)
                  (package-build-working-dir (expand-file-name "elisp"))
                  (package-build-recipes-dir (expand-file-name "recipes")))
             ;; work around issue where macOS adds some "provenance" files
             ;; to any tar files it creates
             (with-environment-variables (("COPYFILE_DISABLE" "true"))
               ;; work around bug where lm-package-requires can't read
               ;; dependencies out of a `.el` file header.
               ;; probably introduced in emacs 30.1
               (cl-letf (((symbol-function 'lm-package-requires) nil))
                 (package-build-archive lib-name t)))
             (let ((entry (assq lib-sym (package-build-archive-alist))))
               (package-install-file (package-build--artifact-file entry))))))))

(apply #'my-byte-compile-local-package argv)

(provide 'byte-compile-local)
;;; byte-compile-local.el ends here
