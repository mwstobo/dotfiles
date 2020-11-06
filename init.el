;;; init --- emacs init
;;; Commentary:
;;; Code:

;;; Constants
(defconst enabled-repos
  '(("melpa" . "https://melpa.org/packages/")))

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

(use-package go-mode
  :mode "\\.go\\'"
  :init
  (add-hook
   'go-mode-hook
   (lambda ()
     (lsp-deferred)
     (lsp-auto-format)
     (setq indent-tabs-mode t))))

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

(use-package pkgbuild-mode
  :mode "/PKGBUILD\\'")

(use-package docker-compose-mode
  :mode "docker-compose[^/]*\\.ya?ml\\'")

(use-package terraform-mode
  :mode "\\.tf\\(vars\\)?\\'")

(use-package scala-mode
  :mode "\\.\\(scala\\|sbt\\|worksheet\\.sc\\)\\'"
  :init
  (add-hook
   'scala-mode-hook 'lsp-deferred))

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
  :mode "Dockerfile\\'")

(use-package nginx-mode
  :mode "nginx\\.conf\\'")

(use-package json-mode
  :mode "\\.json\\'")

(use-package graphql-mode
  :mode "\\.\\(graphql\\|gql\\)\\'")

(use-package sql-indent
  :mode "\\.sql\\'")

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

;;; LSP Mode settings
(use-package lsp-mode
  :commands lsp-deferred lsp-format-buffer lsp-organize-imports
  :config
  (flycheck-mode))

(use-package lsp-completion
  :ensure nil
  :after lsp-mode
  :init
  (setq lsp-completion-provider :capf))

(use-package lsp-pyls
  :ensure nil
  :after lsp-mode
  :defines
  lsp-pyls-plugins-autopep8-enabled
  lsp-pyls-plugins-yapf-enabled
  lsp-pyls-rename-backend
  :init
  (setq lsp-pyls-plugins-autopep8-enabled nil)
  (setq lsp-pyls-plugins-yapf-enabled t)
  (setq lsp-pyls-rename-backend 'rope))

(use-package lsp-solargraph
  :ensure nil
  :after lsp-mode
  :defines
  lsp-solargraph-use-bundler
  lsp-solargraph-library-directories
  :init
  (setq lsp-solargraph-use-bundler t)
  (setq lsp-solargraph-library-directories
	'("/usr/lib/ruby/"
	  "~/.rvm/"
	  "~/.gem/"
	  "/home/matt/code/work/grouped-notifications-service/vendor/bundle/ruby/2.7.0/gems")))

(use-package lsp-java
  :after lsp-mode
  :init
  (setq lsp-java-save-actions-organize-imports t)
  (setq lsp-java-completion-import-order ["java" "javax" "org.springframework"])
  (setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/0.63.0/jdt-language-server-0.63.0-202010141717.tar.gz")
  (setq lsp-java-format-settings-url "https://raw.githubusercontent.com/spring-io/spring-javaformat/v0.0.6/.eclipse/eclipse-code-formatter.xml")
  (setq lsp-java-vmargs '("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-javaagent:/home/matt/code/work/lombok-1.18.8.jar")))

(use-package lsp-metals
  :after lsp-mode)

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
