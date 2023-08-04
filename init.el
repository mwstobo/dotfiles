;;; init --- emacs init
;;; Commentary:
;;; Code:

;;; Init load path
(add-to-list 'load-path (expand-file-name "init/" user-emacs-directory))

;;; elpaca
(require 'init-straight)
(straight-use-package 'use-package)

;;; Configuration from the simple package
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(column-number-mode)

;;; Basic indentation
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;; Visual configuration
(tool-bar-mode 0)
(scroll-bar-mode 0)
(load-theme 'wombat)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq inhibit-startup-screen t)

;;; Performance tuning
(setq gc-cons-threshold 200000000)           ; Performance tuning
(setq read-process-output-max (* 1024 1024)) ; Performance tuning

;;; Tree sitter
(setq treesit-extra-load-path '("~/.emacs.d/tree-sitter/dist"))

;;; Setting up custom
(setq custom-file "~/.emacs.d/custom.el") ; Set, but don't load

;;; Movement control
(global-set-key (kbd "M-e") #'forward-word)

(use-package misc
  :bind ("M-f" . forward-to-word))

;;; Init file access
(defun my-find-init-file ()
  "Open the \"user-init-file\"."
  (interactive)
  (let ((vc-follow-symlinks t))
    (find-file user-init-file)))
(global-set-key (kbd "C-C I") #'my-find-init-file)

;;; Kill excess whitespace when joining lines
(defadvice kill-line (before kill-line-autoreindent activate)
  "Kill excess whitespace when joining lines."
  (when (and (eolp) (not (bolp)))
    (save-excursion
      (forward-char 1)
      (just-one-space 1))))

;;; Backup configuration
(use-package savehist
  :init
  (savehist-mode))

(use-package files
  :custom
  (backup-directory-alist '(("." . "~/.emacs.d/backups")))
  (version-control t)
  (delete-old-versions t)
  (kept-old-versions 2)
  (kept-new-versions 6)
  (backup-by-copying t)
  (require-final-newline t))

(use-package paren
  :custom
  (show-paren-style 'parenthesis)
  (show-paren-context-when-offscreen t)
  :config
  (show-paren-mode))

(use-package elec-pair
  :config
  (electric-pair-mode))

(use-package compile
  :custom
  (compilation-scroll-output 'first-error))

(use-package display-line-numbers
  :init
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))

(use-package auth-source
  :custom
  (auth-sources '(macos-keychain-internet)))

(use-package avy
  :straight t
  :bind ("M-n" . avy-goto-char-timer))

(use-package consult
  :straight t
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x p b" . consult-project-buffer)))

;;; Vertico
(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package vertico-directory
  :after vertico
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :after orderless
  :custom
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-cycle t)
  (corfu-auto t)
  :init
  (global-corfu-mode))

(use-package corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode))

(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

;;; Org
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-startup-indented t)
  (org-hide-leading-stars t)
  (org-export-with-toc)
  (org-agenda-files (directory-files-recursively "~/org" "\\.org$"))
  (org-todo-keywords '("TODO(t)" "NEXT(n)" "|" "WAITING(w)" "DONE(d)")))

(use-package org-agenda
  :bind ("C-c a" . org-agenda))

(use-package verb
  :straight t
  :after org
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

;;; Installed major modes
(use-package js
  :custom
  (js-indent-level 2))

(use-package text-mode
  :init
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'text-mode-hook #'visual-line-mode))

(use-package go-ts-mode
  :mode "\\.go\\'"
  :init
  (add-hook 'go-mode-hook (setq indent-tabs-mode t))
  :custom
  (go-ts-mode-indent-offset 4))

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
  :mode "\\.tf\\(vars\\)?\\'"
  :init
  (add-hook 'terraform-mode-hook #'(lambda () (setq create-lockfiles nil))))

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

(use-package typescript-ts-mode
  :mode "\\.tsx?\\'")

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :custom
  (markdown-command "markdown"))

(use-package protobuf-mode
  :straight t
  :mode ("\\.proto\\'"))

(use-package plantuml-mode
  :straight t
  :mode ("\\.\\(plantuml\\|pum\\|plu\\)\\'")
  :custom
  (plantuml-default-exec-mode 'executable)
  (plantuml-executable-path "/usr/local/bin/plantuml"))

(use-package nftables-mode
  :straight t
  :mode ("/etc/nftables.conf" "\\.nft\\(?:ables\\)?\\'")
  :interpreter ("nft\\(?:ables\\)?"))

(use-package nginx-mode
  :straight t
  :mode ("nginx\\.conf\\'" "/nginx/.+\\.conf\\'"))

(use-package prisma-mode
  :straight (prisma-mode :type git :host github :repo "pimeys/emacs-prisma-mode")
  :mode ("\\.prisma$"))

(use-package jinja2-mode
  :straight t
  :mode ("\\.jinja2\\'" "\\.j2\\'"))

;;; Other useful packages
(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package magit
  :straight t
  :commands magit-status
  :bind (("C-x g" . magit-status)
         ("C-c v" . magit-status)))

(use-package forge
  :straight t
  :after magit)

(use-package git-link
  :straight t
  :commands git-link git-link-commit)

(use-package all-the-icons
  :straight t)

(use-package olivetti
  :straight t
  :commands
  (olivetti-mode)
  :init
  (defvar olivetti--line-spacing nil)
  :custom
  (olivetti-style 'fancy)
  (olivetti-mode-on-hook
   #'(lambda ()
       (setq olivetti--line-spacing line-spacing)
       (setq line-spacing 0.1)))
  (olivetti-mode-off-hook
   #'(lambda ()
       (setq line-spacing olivetti--line-spacing))))

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package topsy
  :straight t)

(use-package mwim
  :straight t
  :bind
  ("C-a" . mwim-beginning)
  ("C-e" . mwim-end))

(use-package npm
  :straight t)

(use-package prettier
  :straight t
  :init
  (add-hook 'typescript-ts-mode-hook #'prettier-mode)
  (add-hook 'js-mode-hook #'prettier-mode)
  (add-hook 'markdown-mode-hook #'prettier-mode)
  (add-hook 'yaml-mode-hook #'prettier-mode))

;;; Language server packages
;;; LSP Mode settings
(use-package lsp-mode
  :straight t
  :commands lsp-deferred lsp-format-buffer lsp-organize-imports
  :custom
  (lsp-completion-provider :none)
  (lsp-signature-render-documentation nil)
  (lsp-rust-clippy-preference "on")
  (lsp-rust-analyzer-proc-macro-enable t)
  (lsp-rust-analyzer-experimental-proc-attr-macros t)
  (lsp-completion-provider :capf)
  (lsp-auto-execute-action nil)
  :init
  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  (defun lsp-mode-install-auto-format-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer nil t)
    (add-hook 'before-save-hook #'lsp-organize-imports nil t))
  (add-hook 'go-ts-mode-hook #'lsp-mode-install-auto-format-hooks)
  (add-hook 'rust-mode-hook #'lsp-mode-install-auto-format-hooks)
  (add-hook 'terraform-mode-hook #'lsp-mode-install-auto-format-hooks)
  (add-hook 'prisma-mode-hook #'(lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil t)))
  (setq lsp-use-plists 1)
  :hook
  ((python-mode rust-mode typescript-ts-mode js-mode go-ts-mode terraform-mode dockerfile-mode tuareg-mode c-mode) . lsp-deferred)
  (lsp-completion-mode . lsp-mode-setup-completion)
  :config
  (setq lsp-json--extra-init-params '(:handledSchemaProtocols ["file" "http" "https"])))

(use-package lsp-pyright
  :straight t
  :after lsp-mode)

(use-package yasnippet
  :straight t
  :hook
  ((lsp-mode eglot-mode-hook) . yas-minor-mode))

(use-package flycheck
  :straight t
  :hook
  (prog-mode . flycheck-mode)
  :init
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33)))
  :custom
  (flycheck-emacs-lisp-load-path 'inherit))

;;; Local config
(use-package init-local
  :if (locate-library "init-local.el"))

(provide 'init)
;;; init.el ends here
