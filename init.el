;;; init --- emacs init
;;; Commentary:
;;; Code:

;;; Basic Configuration

;; package managment
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; indentation
(define-key
  global-map
  (kbd "RET")
  'newline-and-indent)

;; ido
(use-package ido
  :commands ido-everywhere
  :init
  (ido-mode)
  :config
  (ido-everywhere)
  (setq ido-enable-flex-matching t))

;; paren
(use-package paren
  :config
  (show-paren-mode)
  (setq show-paren-style 'mixed))

;; electric pair
(use-package elec-pair
  :config
  (electric-pair-mode))

;; compile
(use-package compile
  :config
  (setq compilation-scroll-output 'first-error))

;; set custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; one offs
(tool-bar-mode 0)                            ; Turn off the tool bar
(load-theme 'wombat)                         ; Use the wombat theme
(set-frame-font "Source Code Pro-11")        ; Set the font
(setq inhibit-startup-screen t)              ; Get rid of the startup screen
(setq gc-cons-threshold 100000000)           ; Performance tuning
(setq tab-width 4)                           ; Tabs display with a width of 4
(setq read-process-output-max (* 1024 1024)) ; Performance tuning

;; line numbers
(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode))

;; org mode
(use-package org
  :config
  (setq org-todo-keywords
	'((sequence "TODO(t)" "IN-PROGRESS(p)" "NEEDS-DEPLOY(y)"
		    "|" "IN-REVIEW(r)" "ON-HOLD(h)" "DONE(d)"))))

;;; Installed major modes
(use-package js
  :config
  (add-hook
   'js-mode-hook (lambda ()
		   (setq js-indent-level 2))))

(use-package go-mode
  :mode "\\.go\\'")

(use-package kotlin-mode
  :mode "\\.kts?\\'")

(use-package pkgbuild-mode
  :mode "/PKGBUILD\\'")

(use-package docker-compose-mode
  :mode "docker-compose[^/]*\\.ya?ml\\'")

(use-package terraform-mode
  :commands terraform-format-on-save-mode
  :mode "\\.tf\\(vars\\)?\\'")

(use-package scala-mode
  :mode "\\.\\(scala\\|sbt\\|worksheet\\.sc\\)\\'"
  :interpreter "scala")

(use-package sbt-mode
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

(use-package sql-indent)

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
  :defines
  lsp-completion-provider
  lsp-pyls-plugins-autopep8-enabled
  lsp-pyls-plugins-yapf-enabled
  lsp-pyls-rename-backend
  lsp-solargraph-use-bundler
  lsp-solargraph-library-directories
  :hook ((go-mode kotlin-mode python-mode js-mode ruby-mode) . lsp-deferred)
  :config
  (flycheck-mode)
  (python-pyls-config)
  (setq lsp-solargraph-use-bundler t)
  (setq lsp-solargraph-library-directories
	'("/usr/lib/ruby/"
	  "~/.rvm/"
	  "~/.gem/"
	  "/home/matt/code/work/grouped-notifications-service/vendor/bundle/ruby/2.7.0/gems"))
  (setq lsp-completion-provider :capf))

(use-package lsp-java
  :after lsp-mode
  :config
  (add-hook 'java-mode-hook 'lsp-deferred)
  (setq lsp-java-completion-import-order ["java" "javax" "org.springframework"])
  (setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/0.63.0/jdt-language-server-0.63.0-202010141717.tar.gz")
  (setq lsp-java-format-settings-url "https://raw.githubusercontent.com/spring-io/spring-javaformat/v0.0.6/.eclipse/eclipse-code-formatter.xml")
  (setq lsp-java-vmargs '("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-javaagent:/home/matt/code/work/lombok-1.18.8.jar")))

(use-package lsp-metals
  :after lsp-mode
  :config
  (add-hook 'scala-mode-hook 'lsp-deferred))

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

(use-package yasnippet
  :config
  (yas-global-mode))

;; go formatting hooks
(defun lsp-go-install-save-hooks ()
  "Add formatting before-save-hooks for go-mode."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; python lsp configs
(defun python-pyls-config ()
  "Python LSP configs."
  (setq lsp-pyls-plugins-autopep8-enabled nil)
  (setq lsp-pyls-plugins-yapf-enabled t)
  (setq lsp-pyls-rename-backend 'rope))

(provide 'init)
;;; init.el ends here
