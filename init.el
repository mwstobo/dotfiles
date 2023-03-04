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
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq inhibit-startup-screen t)
(delete '(vc-mode vc-mode) mode-line-format)

;;; Performance tuning
(setq gc-cons-threshold 200000000)           ; Performance tuning
(setq read-process-output-max (* 1024 1024)) ; Performance tuning

;;; Tree sitter
(setq treesit-extra-load-path '("~/.emacs.d/tree-sitter/dist"))

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

(use-package auth-source
  :custom
  (auth-sources '("secrets:Default keyring" "secrets:Login")))

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
  :straight t
  :after orderless
  :custom
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-cycle t)
  (corfu-auto t)
  :init
  (global-corfu-mode))

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
  :init
  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  (defun lsp-mode-install-auto-format-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer nil t)
    (add-hook 'before-save-hook #'lsp-organize-imports nil t))
  (add-hook 'go-mode-hook #'lsp-mode-install-auto-format-hooks)
  (add-hook 'rust-mode-hook #'lsp-mode-install-auto-format-hooks)
  (add-hook 'terraform-mode-hook #'lsp-mode-install-auto-format-hooks)
  (add-hook 'typescript-ts-mode-hook #'(lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil t)))
  (setq lsp-use-plists 1)
  :hook
  ((python-mode rust-mode typescript-ts-mode js-mode go-mode terraform-mode dockerfile-mode tuareg-mode c-mode) . lsp-deferred)
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
                 (window-height   . 0.33))))

;;; Meow
(use-package meow
  :straight t
  :init
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  :config
  (meow-setup)
  (meow-global-mode)
  (meow-setup-indicator))

;;; Local config
(if (file-readable-p "~/.emacs.d/init-local.el")
    (load "~/.emacs.d/init-local.el"))

(provide 'init)
;;; init.el ends here
