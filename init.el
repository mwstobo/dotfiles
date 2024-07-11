;;; init --- emacs init
;;; Commentary:
;;; Code:

;;; Init load path
(add-to-list 'load-path (expand-file-name "init/" user-emacs-directory))

;;; elpaca
(require 'init-elpaca)
(elpaca elpaca-use-package
        (elpaca-use-package-mode))
(elpaca-wait)

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

(use-package compile
  :custom
  (compilation-scroll-output 'first-error))

(use-package display-line-numbers
  :init
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))

(use-package flymake
  :init
  (add-hook 'prog-mode-hook #'flymake-mode))

(use-package flyspell
  :init
  (add-hook 'org-mode-hook #'flyspell-mode))

(use-package simple
  :init
  (add-hook 'text-mode-hook #'visual-line-mode))

(use-package auth-source
  :custom
  (auth-sources (if (string= system-type "darwin")
                    '(macos-keychain-internet)
                  '("secrets:Default keyring" "secrets:Login"))))

(use-package avy
  :ensure t
  :bind ("M-n" . avy-goto-char-timer))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x p b" . consult-project-buffer)))

;;; Vertico
(use-package vertico
  :ensure (:files (:defaults "extensions/*"))
  :commands vertico-mode
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
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :ensure (:files (:defaults "extensions/*"))
  :after orderless
  :commands global-corfu-mode
  :custom
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-cycle t)
  (corfu-auto t)
  :init
  (global-corfu-mode))

(use-package corfu-popupinfo
  :after corfu
  :commands corfu-popupinfo-mode
  :init
  (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :commands kind-icon-margin-formatter
  :custom
  (kind-icon-default-face 'corfu-default)
  :init
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package marginalia
  :ensure t
  :commands marginalia-mode
  :init
  (marginalia-mode))

;;; Org
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-startup-indented t)
  (org-indent-indentation-per-level 0)
  (org-indent-mode-turns-on-hiding-stars nil)
  (org-agenda-files (directory-files-recursively "~/org" "\\.org$"))
  (org-todo-keywords '("TODO(t)" "NEXT(n)" "|" "WAITING(w)" "DONE(d)")))

(use-package org-agenda
  :bind ("C-c a" . org-agenda))

(use-package verb
  :ensure t
  :after org
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

;;; Installed major modes
(use-package elisp-mode
  :init
  (setq elisp-flymake-byte-compile-load-path load-path))

(use-package js
  :mode ("\\.js[mx]?\\'" . js-mode)
  :bind (:map js-mode-map
              ("M-." . nil))
  :custom
  (js-indent-level 2))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :init
  (add-hook 'go-mode-hook (setq indent-tabs-mode t)))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

(use-package tuareg
  :ensure t
  :mode
  ("\\.ml[ip]?\\'" . tuareg-mode)
  ("\\.opam\\'" . tuareg-opam-mode))

(use-package dune
  :mode ("\\(?:\\`\\|/\\)dune\\(?:\\.inc\\|\\-project\\)?\\'" . dune-mode)
  :ensure t)

(use-package kotlin-mode
  :ensure t
  :mode "\\.kts?\\'")

(use-package pkgbuild-mode
  :ensure t
  :mode "/PKGBUILD\\'")

(use-package docker-compose-mode
  :ensure t
  :mode "docker-compose[^/]*\\.ya?ml\\'")

(use-package terraform-mode
  :ensure t
  :mode "\\.tf\\(vars\\)?\\'"
  :init
  (add-hook 'terraform-mode-hook #'(lambda () (setq create-lockfiles nil))))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package graphql-mode
  :ensure t
  :mode "\\.\\(graphql\\|gql\\)\\'")

(use-package sql-indent
  :ensure t
  :mode ("\\.sql\\'" . sqlind-minor-mode))

(use-package typescript-ts-mode
  :mode "\\.ts\\'")

(use-package tsx-ts-mode
  :mode "\\.tsx\\'")

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :custom
  (markdown-command "markdown"))

(use-package protobuf-mode
  :ensure t
  :mode ("\\.proto\\'"))

(use-package plantuml-mode
  :ensure t
  :mode ("\\.\\(plantuml\\|pum\\|plu\\)\\'")
  :custom
  (plantuml-default-exec-mode 'executable)
  (plantuml-executable-path "/usr/local/bin/plantuml"))

(use-package nftables-mode
  :ensure t
  :mode ("/etc/nftables.conf" "\\.nft\\(?:ables\\)?\\'")
  :interpreter ("nft\\(?:ables\\)?"))

(use-package nginx-mode
  :ensure t
  :mode ("nginx\\.conf\\'" "/nginx/.+\\.conf\\'"))

(use-package jinja2-mode
  :ensure t
  :mode ("\\.jinja2\\'" "\\.j2\\'"))

;;; Other useful packages
(use-package which-key
  :ensure t
  :commands which-key-mode
  :init
  (which-key-mode))

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :after transient
  :commands magit-status
  :bind (("C-x g" . magit-status)
         ("C-c v" . magit-status)))

(use-package git-link
  :ensure t
  :commands git-link git-link-commit)

(use-package all-the-icons
  :ensure t)

(use-package olivetti
  :ensure t
  :commands olivetti-mode
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

(use-package topsy
  :ensure t)

(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning)
  ("C-e" . mwim-end))

(use-package prettier
  :ensure t
  :commands prettier-mode
  :init
  (add-hook 'typescript-ts-mode-hook #'prettier-mode)
  (add-hook 'tsx-ts-mode-hook #'prettier-mode)
  (add-hook 'js-mode-hook #'prettier-mode)
  (add-hook 'markdown-mode-hook #'prettier-mode)
  (add-hook 'yaml-mode-hook #'prettier-mode))

(use-package yasnippet
  :ensure t
  :commands yas-reload-all yas-minor-mode
  :init
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package eglot
  :commands eglot eglot-format-buffer
  :init
  (add-hook 'rust-mode-hook #'(lambda () (add-hook 'eglot-managed-mode-hook #'(lambda () (add-hook 'before-save-hook #'eglot-format-buffer)))))
  (add-hook 'go-mode-hook #'(lambda () (add-hook 'eglot-managed-mode-hook #'(lambda () (add-hook 'before-save-hook #'eglot-format-buffer)))))
  :config
  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve"))))

;;; Local config
(use-package init-local
  :if (locate-library "init-local.el"))

(provide 'init)
;;; init.el ends here
