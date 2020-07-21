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
  :mode "\\.go\\'"
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))
