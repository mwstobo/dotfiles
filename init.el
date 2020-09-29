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
  (ido-everywhere)
  :config
  (setq ido-enable-flex-matching t))

;; set custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; one offs
(tool-bar-mode 0)                            ; Turn off the tool bar
(load-theme 'wombat)                         ; Use the wombat theme
(set-frame-font "Source Code Pro-13")        ; Set the font
(global-display-line-numbers-mode)           ; Always display line numbers
(setq inhibit-startup-screen t)              ; Get rid of the startup screen
(setq gc-cons-threshold 100000000)           ; Performance tuning
(setq tab-width 4)                           ; Tabs display with a width of 4
(setq read-process-output-max (* 1024 1024)) ; Performance tuning

;; org mode
(require 'org)
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "ON-HOLD" "WAITING" "DONE")))

;; installed packages
;; (setq package-selected-packages '(json-mode nginx-mode git-link sbt-mode lsp-metals scala-mode terraform-mode kotlin-mode docker-compose-mode lsp-treemacs lsp-java pkgbuild-mode dockerfile-mode magit yasnippet company which-key flycheck lsp-mode go-mode use-package))

;;; Installed major modes
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

(use-package org-jira
  :defines jiralib-url
  :config
  (setq jiralib-url "https://500pxinc.atlassian.net"))

;;; LSP Mode settings
(use-package lsp-mode
  :commands lsp-deferred lsp-format-buffer lsp-organize-imports
  :defines lsp-completion-provider
  :hook ((go-mode kotlin-mode) . lsp-deferred)
  :config
  (flycheck-mode)
  (setq lsp-completion-provider :capf))

(use-package lsp-java
  :hook (java-mode . lsp-deferred)
  :config
  (setq lsp-java-completion-import-order ["java" "javax" "org.springframework"])
  (setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/0.60.0/jdt-language-server-0.60.0-202008311720.tar.gz")
  (setq lsp-java-format-settings-url "https://raw.githubusercontent.com/spring-io/spring-javaformat/v0.0.6/.eclipse/eclipse-code-formatter.xml")
  (setq lsp-java-vmargs '("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-javaagent:/home/matt/code/work/lombok-1.18.8.jar")))

(use-package lsp-metals
  :hook (scala-mode . lsp-deferred))

(use-package lsp-treemacs
  :commands (lsp-treemacs-errors-list))

(use-package flycheck
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
		 (display-buffer-reuse-window
		  display-buffer-in-side-window)
		 (side            . bottom)
		 (reusable-frames . visible)
		 (window-height   . 0.33))))

(use-package company
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2))

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
