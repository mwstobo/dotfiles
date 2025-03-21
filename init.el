;;; init --- emacs init
;;; Commentary:
;;; Code:

(defun my-find-init-file ()
    "Open the \"user-init-file\"."
    (interactive)
    (let ((vc-follow-symlinks t))
      (find-file user-init-file)))

(use-package emacs
  :init
  (add-to-list 'load-path (expand-file-name "init/" user-emacs-directory))
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (setq-default tab-width 4)
  (setq-default mode-line-format (delete '(vc-mode vc-mode) mode-line-format))
  (setq gc-cons-threshold 200000000)
  (setq read-process-output-max (* 1024 1024))
  (setq treesit-extra-load-path '("~/.emacs.d/tree-sitter/dist"))
  (global-set-key (kbd "M-e") #'forward-word)
  (global-set-key (kbd "C-C I") #'my-find-init-file))

(use-package simple
  :hook
  (before-save . delete-trailing-whitespace)
  (after-init . column-number-mode)
  :init
  (setq-default indent-tabs-mode nil))

(use-package startup
  :init
  (setq inhibit-startup-screen t))

(use-package custom
  :init
  (load-theme 'wombat))

(use-package cus-edit
  :init
  ; Set, but don't load
  (setq custom-file "~/.emacs.d/custom.el"))

(use-package misc
  :bind ("M-f" . forward-to-word))

(use-package package
  :init
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(use-package xref
  :custom
  (xref-search-program #'ripgrep))

(use-package isearch
  :custom
  (isearch-lazy-count t))

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

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package display-line-numbers
  :init
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))

(use-package flymake
  :init
  (add-hook 'prog-mode-hook #'flymake-mode))

(use-package flyspell
  :init
  (add-hook 'org-mode-hook #'flyspell-mode)
  (add-hook 'prod-mode-hook #'flyspell-prog-mode))

(use-package simple
  :init
  (add-hook 'text-mode-hook #'visual-line-mode))

(use-package grep
  :custom
  (grep-find-template "find -H <D> <X> -type f <F> -exec rg <C> --no-heading -H <R> \\{\\} +"))

(use-package auth-source
  :custom
  (auth-sources (if (string= system-type "darwin")
                    '(macos-keychain-internet)
                  '("secrets:Default keyring" "secrets:Login"))))

(use-package project
  :after xref
  :commands project-find-regexp-with-unique-buffer
  :init
  (defun project-find-regexp-with-unique-buffer (orig-fun &rest args)
    (let ((xref-buffer-name (format "%s %s" xref-buffer-name (car args))))
      (apply orig-fun args)))
  (advice-add 'project-find-regexp :around #'project-find-regexp-with-unique-buffer))

(use-package rg
  :ensure t)

(use-package ring
  :commands ring-ref)

(use-package moody
  :ensure t
  :commands
  moody-replace-mode-line-front-space
  moody-replace-mode-line-buffer-identification
  moody-replace-eldoc-minibuffer-message-function
  :custom
  (moody-mode-line-height 20)
  :init
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package avy
  :ensure t
  :commands avy-action-copy-whole-line
  :bind ("M-j" . avy-goto-char-timer)
  :init
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  (setf (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line)
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)
  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x p b" . consult-project-buffer)))

;;; Vertico
(use-package vertico
  :ensure t
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
  :ensure t
  :after orderless
  :hook (prog-mode . corfu-mode)
  :custom
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-cycle t)
  (corfu-auto t))

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
  :commands gofmt-before-save
  :mode "\\.go\\'"
  :init
  (add-hook 'go-mode-hook (setq indent-tabs-mode t))
  (add-hook 'go-mode-hook #'(lambda () (add-hook 'before-save-hook #'gofmt-before-save)))
  :custom
  (gofmt-command "goimports"))

(use-package rust-mode
  :ensure t
  :commands rust-enable-format-on-save
  :mode "\\.rs\\'"
  :init
  (add-hook 'rust-mode-hook #'rust-enable-format-on-save))

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

(use-package bazel
  :ensure t)

;;; Other useful packages
(use-package which-key
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
  :commands git-link git-link-commit
  :custom
  (git-link-open-in-browser t))

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
  :ensure t
  :commands topsy-mode
  :init
  (add-hook 'prog-mode-hook #'topsy-mode)
  (add-hook 'magit-section-mode-hook #'topsy-mode))

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
  (add-hook 'js-mode-hook #'prettier-mode))

(use-package yasnippet
  :ensure t
  :commands yas-reload-all yas-minor-mode
  :init
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package eglot
  :commands eglot eglot-format-buffer eglot-code-actions xref-find-references-with-eglot
  :init
  (defun xref-find-references-with-eglot (orig-fun &rest args)
    (if (bound-and-true-p eglot--managed-mode)
        (let ((xref-buffer-name (format "%s %s" xref-buffer-name (symbol-at-point))))
          (apply orig-fun args))
      (apply orig-fun args)))
  (advice-add 'xref-find-references :around #'xref-find-references-with-eglot)
  :config
  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))
  (add-to-list 'eglot-server-programs
               '((go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode) .
                 ("gopls"
                  :initializationOptions
                  (:usePlaceholders
                   t
                   :staticcheck
                   t
                   :hints (
                           :parameterNames
                           t
                           :compositeLiteralTypes
                           t)
                   ))))
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) .
                 ("rust-analyzer"
                  :initializationOptions
                  (:check (:command "clippy"))))))


(use-package dape
  :ensure t)

;;; Local config
(use-package init-local
  :if (locate-library "init-local.el"))

(provide 'init)
;;; init.el ends here
