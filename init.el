;;; init --- emacs init
;;; Commentary:
;;; Code:
;; set up package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; set custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; newline and indentx
(define-key global-map (kbd "RET") 'newline-and-indent)

;; install use-package
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; always install packages
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; major modes
(use-package go-mode
  :mode "\\.go\\'")

(use-package kotlin-mode
  :mode "\\.kts?\\'")

(use-package pkgbuild-mode
  :mode "/PKGBUILD\\'")

(use-package docker-compose-mode
  :mode "docker-compose[^/]*\\.ya?ml\\'")

(use-package terraform-mode
  :commands (terraform-format-on-save-mode)
  :mode "\\.tf\\(vars\\)?\\'")

;; useful packages
(use-package which-key
  :init (which-key-mode))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;; lsp-mode
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(use-package lsp-mode
  :commands (lsp-deferred lsp-format-buffer lsp-organize-imports)
  :hook ((go-mode kotlin-mode) . lsp-deferred))

(use-package lsp-java
  :hook (java-mode . lsp-deferred))

(use-package lsp-treemacs
  :commands (lsp-treemacs-errors-list))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package company
  :init (global-company-mode))

(use-package yasnippet
  :init (yas-global-mode))

;; go formatting hooks
(defun lsp-go-install-save-hooks ()
  "Add formatting before-save-hooks for go-mode."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(provide 'init)
;;; init.el ends here
