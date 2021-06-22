;;; init --- emacs init
;;; Commentary:
;;; Code:

;;; Constants
(defconst enabled-repos
  '(("melpa" . "https://melpa.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")))

;;; Utility functions
(defun ensure-package (package)
  "Ensure PACKAGE is installed."
  (when (not (package-installed-p package))
    (package-refresh-contents)
    (package-install package)))

(defun lsp-auto-format ()
  "Enable formatting and organization on save."
  (add-hook 'before-save-hook 'lsp-organize-imports)
  (add-hook 'before-save-hook 'lsp-format-buffer))

;;; Configuration from the simple package
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Basic indentation
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;; Visual configuration
(tool-bar-mode 0)
(scroll-bar-mode 0)
(load-theme 'wombat)
(set-frame-font "Source Code Pro-11")
(toggle-frame-maximized)
(setq inhibit-startup-screen t)

;;; Performance tuning
(setq gc-cons-threshold 100000000)           ; Performance tuning
(setq read-process-output-max (* 1024 1024)) ; Performance tuning

;;; Setting up package and use-package
(require 'package)
(dolist (repo enabled-repos) (add-to-list 'package-archives repo t))
(package-initialize)

(ensure-package 'use-package)
(require 'use-package)
(setq use-package-compute-statistics t)

(use-package use-package-ensure
  :config
  (setq use-package-always-ensure t))

;;; Setting up custom
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; Backup configuration
(use-package files
  :ensure nil
  :config
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
        version-control t
        delete-old-versions t
        kept-old-versions 2
        kept-new-versions 6
        backup-by-copying t))

;;; Built-in Emacs packages
(use-package ido
  :commands ido-everywhere
  :init
  (ido-mode)
  :config
  (ido-everywhere)
  (setq ido-enable-flex-matching t))

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
  (setq org-todo-keywords
	'("TODO(t)" "IN-PROGRESS(p)" "NEEDS-DEPLOY(y)"
	  "|" "IN-REVIEW(r)" "ON-HOLD(h)" "DONE(d)")))

;;; Installed major modes
(use-package cc-mode
  :init
  (add-hook
   'java-mode-hook
   (lambda ()
     (lsp-deferred)
     (setq c-basic-offset 4)
     (setq indent-tabs-mode t))))

(use-package js
  :init
  (add-hook
   'js-mode-hook
   (lambda ()
     (lsp-deferred)
     (setq js-indent-level 2))))

(use-package python
  :ensure nil
  :init
  (add-hook 'python-mode-hook 'lsp-deferred))

(use-package ruby-mode
  :ensure nil
  :init
  (add-hook 'ruby-mode-hook 'lsp-deferred))

(use-package text-mode
  :ensure nil
  :init
  (add-hook
   'text-mode-hook
   (lambda ()
     (flyspell-mode)
     (visual-line-mode))))

(use-package go-mode
  :mode "\\.go\\'"
  :init
  (add-hook
   'go-mode-hook
   (lambda ()
     (lsp-deferred)
     (lsp-auto-format)
     (setq indent-tabs-mode t))))

(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (add-hook
   'rust-mode-hook
   (lambda ()
     (lsp-deferred)
     (lsp-auto-format)
     (lsp-lens-mode)
     (setq indent-tabs-mode t))))

(use-package fsharp-mode
  :mode "\\.fs[iylx]?\\'"
  :init
  (add-hook 'fsharp-mode-hook 'lsp-deferred))

(use-package tuareg
  :mode ("\\.ml[ip]?\\'" . tuareg-mode)
  :init
  (add-hook
   'tuareg-mode-hook
   (lambda ()
     (lsp-deferred)
     (lsp-auto-format))))

(use-package tuareg-jbuild
  :ensure nil
  :mode ("/dune\\'" . tuareg-jbuild-mode))

(use-package tuareg-opam
  :ensure nil
  :mode ("\\.opam\\'" . tuareg-opam-mode))

(use-package kotlin-mode
  :mode "\\.kts?\\'"
  :init
  (add-hook 'kotlin-mode-hook 'lsp-deferred))

(use-package elm-mode
  :mode "\\.elm\\'"
  :init
  (add-hook 'elm-mode-hook 'lsp-deferred))

(use-package pkgbuild-mode
  :mode "/PKGBUILD\\'")

(use-package docker-compose-mode
  :mode "docker-compose[^/]*\\.ya?ml\\'")

(use-package terraform-mode
  :mode "\\.tf\\(vars\\)?\\'")

(use-package scala-mode
  :mode "\\.\\(scala\\|sbt\\|worksheet\\.sc\\)\\'"
  :init
  (add-hook 'scala-mode-hook 'lsp-deferred))

(use-package clojure-mode
  :mode "\\.\\(clj\\|dtm\\|edn\\)\\'"
  :init
  (add-hook 'clojure-mode-hook 'lsp-deferred))

(use-package sbt-mode
  :after scala-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :init
  (add-hook 'dockerfile-mode-hook 'lsp-deferred))

(use-package nginx-mode
  :mode "nginx\\.conf\\'")

(use-package json-mode
  :mode "\\.json\\'")

(use-package graphql-mode
  :mode "\\.\\(graphql\\|gql\\)\\'")

(use-package sql-indent
  :mode ("\\.sql\\'" . sqlind-minor-mode))

;;; Other useful packages
(use-package which-key
  :init (which-key-mode))

(use-package magit
  :commands magit-status
  :bind (("C-x g" . magit-status)))

(use-package forge
  :after magit)

(use-package git-link
  :commands git-link git-link-commit)

(use-package github-review
  :after forge)

(use-package reformatter
  :commands
  reformatter--do-region)

(reformatter-define google-java-format
  :program "google-java-format"
  :args '("-")
  :lighter " GJF"
  :group 'google-java-format)

(use-package all-the-icons)

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "Markdown.pl"))

;;; LSP Mode settings
(use-package lsp-mode
  :commands lsp-deferred lsp-format-buffer lsp-organize-imports
  :init
  (setq lsp-signature-render-documentation nil)
  :config
  (flycheck-mode))

(use-package lsp-ui
  :after lsp-mode
  :init (setq lsp-ui-doc-position 'top))

(use-package lsp-ido
  :ensure nil
  :after lsp-mode)

(use-package lsp-completion
  :ensure nil
  :after lsp-mode
  :init
  (setq lsp-completion-provider :capf))

(use-package lsp-rust
  :ensure nil
  :after lsp-mode
  :init
  (setq lsp-rust-clippy-preference "on"))

(use-package lsp-pyright
  :ensure t
  :after lsp-mode)

(use-package lsp-solargraph
  :ensure nil
  :after lsp-mode
  :defines
  lsp-solargraph-use-bundler
  lsp-solargraph-library-directories
  :init
  (setq lsp-solargraph-use-bundler t))

(use-package lsp-dockerfile
  :ensure nil
  :after lsp-mode)

(use-package lsp-java
  :after lsp-mode
  :init
  (setq lsp-java-completion-import-order ["java" "javax" "org.springframework"])
  (setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/0.70.0/jdt-language-server-0.70.0-202103051608.tar.gz")
  (setq lsp-java-format-settings-url "https://raw.githubusercontent.com/spring-io/spring-javaformat/v0.0.6/.eclipse/eclipse-code-formatter.xml")
  (setq lsp-java-vmargs '("-noverify" "-Xmx2G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-javaagent:/home/matt/code/work/lombok-1.18.8.jar")))

(use-package lsp-metals
  :after lsp-mode)

(use-package lsp-rust
  :ensure nil
  :after lsp-mode
  :config
  (setq lsp-rust-clippy-preference "on"))

(use-package lsp-treemacs
  :after lsp-mode
  :commands (lsp-treemacs-errors-list))

(use-package flycheck
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
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2))

(use-package company-dabbrev
  :ensure nil
  :after company
  :init
  (setq company-dabbrev-ignore-case nil))

(use-package yasnippet
  :config
  (yas-global-mode))

(provide 'init)
;;; init.el ends here
