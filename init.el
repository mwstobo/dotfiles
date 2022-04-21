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
(column-number-mode)

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
(setq gc-cons-threshold 200000000)           ; Performance tuning
(setq read-process-output-max (* 1024 1024)) ; Performance tuning

;;; Setting up package and use-package
(straight-use-package 'use-package)
(require 'use-package)
(setq use-package-compute-statistics t)

;;; Setting up custom
(setq custom-file "~/.emacs.d/custom.el") ; Set, but don't load

;;; Init file access
(defun my-find-init-file ()
  "Open the 'user-init-file'."
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
  (backup-by-copying t))

(use-package paren
  :custom
  (show-paren-style 'mixed)
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

;;; Tree sitter
(use-package tree-sitter-langs
  :straight t)

(use-package tree-sitter
  :after tree-sitter-langs
  :straight t
  :init
  (dolist
      (hook-symbol
       (list 'go-mode-hook 'rust-mode-hook 'js-mode-hook 'python-mode-hook))
    (add-hook hook-symbol #'tree-sitter-mode))
  (add-hook 'tree-sitter-mode-hook #'tree-sitter-hl-mode))

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

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

;;; Org
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-export-with-toc nil)
  (org-agenda-files '("~/.emacs.d/gtd/gtd.org" "~/.emacs.d/gtd/inbox.org"))
  (org-todo-keywords '("TODO(t)" "NEXT(n)" "|" "WAITING(w)" "DONE(d)")))

(use-package org-capture
  :bind
  ("C-c c" . org-capture)
  ("C-c i" . org-capture-inbox)
  :config
  (defun org-capture-inbox ()
    (interactive)
    (org-capture nil "i"))
  :custom
  (org-capture-bookmark nil)
  (org-capture-templates
   '(("i" "Inbox" entry (file "~/.emacs.d/gtd/inbox.org") "* TODO %?\nCaptured on %U"))))

(use-package org-refile
  :after org
  :custom
  (org-refile-targets '(("~/.emacs.d/gtd/gtd.org" :level . 1))))

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
  :custom
  (org-agenda-hide-tags-regexp ".")
  (org-agenda-custom-commands
   '(("g" "GTD"
      ((agenda "")
       (todo
        "NEXT"
        ((org-agenda-overriding-header "Tasks")))
       (tags-todo
        "inbox"
        ((org-agenda-overriding-header "Inbox")))))
     ("n" "Next"
      ((tags-todo
        "projects"
        ((org-agenda-overriding-header "Next tasks for projects: ")
         (org-agenda-skip-function #'org-gtd-skip-all-but-next)))
       (tags-todo
        "simple"
        ((org-agenda-overriding-header "Next simple tasks: "))))
      ((org-agenda-prefix-format "%(car (last (org-get-outline-path))): "))))))

;;; Installed major modes
(use-package prog-mode
  :init
  (defun set-show-trailing-whitespace ()
    (setq show-trailing-whitespace t))
  (add-hook 'prog-mode-hook #'set-show-trailing-whitespace))

(use-package js
  :custom
  (js-indent-level 2))

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
  :custom
  (typescript-indent-level 2))

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

(use-package avy
  :straight t
  :bind
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("M-g f" . avy-goto-line)
  ("M-g w" . avy-goto-word-1)
  ("M-g e" . avy-goto-word-0)
  :config
  (avy-setup-default))

;;; Language server packages
;;; LSP Mode settings
(use-package lsp-mode
  :straight t
  :commands lsp-deferred lsp-format-buffer lsp-organize-imports
  :hook
  ((python-mode rust-mode typescript-mode js-mode go-mode) . lsp-deferred)
  :init
  (defun lsp-mode-install-auto-format-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer nil t)
    (add-hook 'before-save-hook #'lsp-organize-imports nil t))
  (add-hook 'go-mode-hook #'lsp-mode-install-auto-format-hooks)
  (add-hook 'rust-mode-hook #'lsp-mode-install-auto-format-hooks)
  :custom
  (lsp-use-plists t)
  (lsp-signature-render-documentation nil)
  (lsp-rust-clippy-preference "on")
  (lsp-completion-provider :capf)
  (lsp-kotlin-external-sources-auto-convert-to-kotlin nil))

(use-package lsp-pyright
  :straight t
  :after lsp-mode)

(use-package company
  :straight t
  :hook
  (prog-mode . company-mode)
  :custom
  (company-dabbrev-ignore-case 'keep-prefix)
  (company-dabbrev-downcase nil)
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 1))

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
                 (window-height   . 0.33))))

;;; Local config
(if (file-readable-p "~/.emacs.d/init-local.el")
    (load "~/.emacs.d/init-local.el"))

(provide 'init)
;;; init.el ends here
