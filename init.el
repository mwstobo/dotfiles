;;; init --- emacs init
;;; Commentary:
;;; Code:

;;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Configuration from the simple package
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Basic indentation
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;; Visual configuration
(tool-bar-mode 0)
(scroll-bar-mode 0)
(load-theme 'wombat)
(set-frame-font "Source Code Pro-12")
(toggle-frame-maximized)
(setq inhibit-startup-screen t)

;;; Performance tuning
(setq gc-cons-threshold 100000000)           ; Performance tuning
(setq read-process-output-max (* 1024 1024)) ; Performance tuning

;;; Setting up package and use-package
(straight-use-package 'use-package)
(require 'use-package)
(setq use-package-compute-statistics t)

;;; Setting up custom
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; Backup configuration
(use-package files
  :config
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
        version-control t
        delete-old-versions t
        kept-old-versions 2
        kept-new-versions 6
        backup-by-copying t))

;;; Built-in Emacs packages
(use-package icomplete
  :init
  (fido-vertical-mode))

;; (use-package ido
;;   :commands ido-everywhere
;;   :init
;;   (ido-mode)
;;   :config
;;   (ido-everywhere)
;;   (setq ido-enable-flex-matching t))

(use-package paren
  :config
  (show-paren-mode)
  (setq show-paren-style 'mixed))

(use-package elec-pair
  :config
  (electric-pair-mode))

(use-package compile
  :config
  (setq compilation-scroll-output 'first-error))

(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-agenda-files
        '("~/sync/gtd/gtd.org"))
  (setq org-todo-keywords
	'("TODO(t)" "|" "WAITING(w)" "DONE(d)")))

(use-package org-capture
  :ensure nil
  :bind ("C-c c" . org-capture)
  :config
  (setq org-capture-bookmark nil)
  (setq org-capture-templates
        '(("t" "Todo [inbox]" entry (file "~/sync/gtd/inbox.org") "* TODO %i%?"))))

(use-package org-refile
  :ensure nil
  :config
  (setq org-refile-targets
        '(("~/sync/gtd/gtd.org" :level . 1))))

(use-package org-agenda
  :ensure nil
  :bind ("C-c a" . org-agenda)
  :config
  (setq org-agenda-custom-commands
        '(("wa" "Work [all]"
           tags-todo "work")
          ("wn" "Work [next]"
           ((tags-todo "work+@next")
            (tags-todo "work+simple")
            (todo "WAITING")))
          ("pa" "Personal [all]"
           tags-todo "personal")
          ("pn" "Personal [next]"
           ((tags-todo "personal+@next")
            (tags-todo "personal+simple")
            (todo "WAITING"))))))

;;; Installed major modes
(use-package cc-mode
  :init
  (add-hook
   'java-mode-hook
   (lambda ()
     (setq c-basic-offset 4)
     (setq indent-tabs-mode t))))

(use-package js
  :init
  (add-hook
   'js-mode-hook
   (lambda ()
     (setq js-indent-level 2))))

(use-package text-mode
  :init
  (add-hook
   'text-mode-hook
   (lambda ()
     (flyspell-mode)
     (visual-line-mode))))

(use-package go-mode
  :straight t
  :mode "\\.go\\'"
  :init
  (add-hook
   'go-mode-hook
   (lambda ()
     (setq indent-tabs-mode t))))

(use-package rust-mode
  :straight t
  :mode "\\.rs\\'")

(use-package tuareg
  :straight t
  :mode ("\\.ml[ip]?\\'" . tuareg-mode))

(use-package tuareg-opam
  :mode ("\\.opam\\'" . tuareg-opam-mode))

(use-package dune
  :straight t)

(use-package pkgbuild-mode
  :straight t
  :mode "/PKGBUILD\\'")

(use-package docker-compose-mode
  :straight t
  :mode "docker-compose[^/]*\\.ya?ml\\'")

(use-package terraform-mode
  :straight t
  :mode "\\.tf\\(vars\\)?\\'")

(use-package dockerfile-mode
  :straight t
  :mode "Dockerfile\\'")

(use-package json-mode
  :straight t
  :mode "\\.json\\'")

(use-package graphql-mode
  :straight t
  :mode "\\.\\(graphql\\|gql\\)\\'")

(use-package sql-indent
  :straight t
  :mode ("\\.sql\\'" . sqlind-minor-mode))

(use-package typescript-mode
  :straight t
  :mode "\\.tsx?\\'"
  :config
  (setq typescript-indent-level 2))

;;; Other useful packages
(use-package which-key
  :straight t
  :init (which-key-mode))

(use-package magit
  :straight t
  :commands magit-status
  :bind (("C-x g" . magit-status)))

(use-package forge
  :straight t
  :after magit)

(use-package git-link
  :straight t
  :commands git-link git-link-commit)

(use-package all-the-icons
  :straight t)

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "Markdown.pl"))

;;; Language server packages
(use-package eglot
  :straight t
  :commands (eglot eglot-ensure)
  :hook
  ((rust-mode python-mode) . eglot-ensure)
  :init
  (defun eglot-install-format-hooks ()
    (add-hook 'before-save-hook #'eglot-format-buffer nil t))
  (add-hook 'rust-mode-hook #'eglot-install-format-hooks)
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer"))))

(use-package flycheck
  :straight t
  :hook
  (emacs-lisp-mode . flycheck-mode)
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
		 (display-buffer-reuse-window
		  display-buffer-in-side-window)
		 (side            . bottom)
		 (reusable-frames . visible)
		 (window-height   . 0.33))))

(use-package company
  :straight t
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2))

(use-package company-dabbrev
  :after company
  :init
  (setq company-dabbrev-ignore-case nil))

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode))

(provide 'init)
;;; init.el ends here
