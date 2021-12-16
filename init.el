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
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;; Basic indentation
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;; Visual configuration
(tool-bar-mode 0)
(scroll-bar-mode 0)
(load-theme 'wombat)
(toggle-frame-maximized)
(setq inhibit-startup-screen t)
(delete '(vc-mode vc-mode) mode-line-format)

;;; Performance tuning
(setq gc-cons-threshold 100000000)           ; Performance tuning
(setq read-process-output-max (* 1024 1024)) ; Performance tuning

;;; Setting up package and use-package
(straight-use-package 'use-package)
(add-to-list 'elisp-flymake-byte-compile-load-path "~/.emacs.d/straight/repos/use-package")
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
  :config
  (fido-vertical-mode))

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
  :init
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))

;;; Org
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-agenda-files
        '("~/.emacs.d/gtd/gtd.org"))
  (setq org-todo-keywords
	    '("TODO(t)" "|" "WAITING(w)" "DONE(d)")))

(use-package org-capture
  :bind ("C-c c" . org-capture)
  :init
  (setq org-capture-bookmark nil)
  (setq org-capture-templates
        '(("t" "Todo [inbox]" entry (file "~/.emacs.d/gtd/inbox.org") "* TODO %i%?"))))

(use-package org-refile
  :after org
  :init
  (setq org-refile-targets
        '(("~/.emacs.d/gtd/gtd.org" :level . 1))))

(use-package org-agenda
  :bind ("C-c a" . org-agenda)
  :init
  (defun org-gtd-distance-to-header ()
    (save-excursion
      (push-mark)
      (org-up-element)
      (count-lines (point) (mark))))
  (defun org-gtd-skip-all-but-next ()
    (if (eq (org-gtd-distance-to-header) 1) nil (outline-next-heading)))
  (setq
   org-agenda-custom-commands
   '(("n" "Next"
      ((tags-todo
        "projects"
        ((org-agenda-overriding-header "Next tasks for projects: ")
         (org-agenda-skip-function #'org-gtd-skip-all-but-next)))
       (tags-todo
        "simple"
        ((org-agenda-overriding-header "Next simple tasks: "))))
      ((org-agenda-prefix-format "%(car (last (org-get-outline-path))): "))))))

;;; Installed major modes
(use-package elisp-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'flymake-mode))

(use-package js
  :init
  (setq js-indent-level 2))

(use-package text-mode
  :init
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'text-mode-hook #'visual-line-mode))

(use-package go-mode
  :straight t
  :mode "\\.go\\'"
  :init
  (add-hook 'go-mode-hook (setq indent-tabs-mode t)))

(use-package rust-mode
  :straight t
  :mode "\\.rs\\'")

(use-package tuareg
  :straight t
  :mode
  ("\\.ml[ip]?\\'" . tuareg-mode)
  ("\\.opam\\'" . tuareg-opam-mode))

(use-package dune
  :mode ("\\(?:\\`\\|/\\)dune\\(?:\\.inc\\|\\-project\\)?\\'" . dune-mode)
  :straight t)

(use-package kotlin-mode
  :straight t
  :mode "\\.kts?\\'")

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

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "Markdown.pl"))

(use-package protobuf-mode
  :straight t
  :mode ("\\.proto\\'"))

;;; Other useful packages
(use-package which-key
  :straight t
  :config
  (which-key-mode))

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

;;; Language server packages
;;; LSP Mode settings
(use-package lsp-mode
  :straight t
  :commands lsp-deferred lsp-format-buffer lsp-organize-imports
  :init
  (setq lsp-signature-render-documentation nil)
  (setq lsp-rust-clippy-preference "on")
  (setq lsp-completion-provider :capf)
  :config
  (flycheck-mode))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :init
  (setq lsp-ui-doc-position 'top))

(use-package lsp-pyright
  :straight t
  :after lsp-mode)

(use-package lsp-treemacs
  :straight t
  :after lsp-mode)

(use-package company
  :straight t
  :hook
  (prog-mode . company-mode)
  :init
  (setq company-dabbrev-ignore-case nil)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2))

(use-package yasnippet
  :straight t
  :hook
  (prog-mode . yas-minor-mode))

;;; eglot
(use-package eglot
  :straight t
  :hook
  ((kotlin-mode python-mode rust-mode typescript-mode js-mode) . eglot-ensure)
  :init
  (setq eglot-connect-timeout 120)
  (defun eglot-install-format-hooks ()
    (add-hook 'before-save-hook #'eglot-format-buffer nil t))
  (add-hook 'rust-mode-hook #'eglot-install-format-hooks)
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(typescript-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer"))))

;;; Local config
(if (file-readable-p "~/.emacs.d/init-local.el")
    (load "~/.emacs.d/init-local.el"))

(provide 'init)
;;; init.el ends here
