(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (wombat)))
 '(gc-cons-threshold 100000000)
 '(global-display-line-numbers-mode t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(inhibit-startup-screen t)
 '(lsp-java-format-settings-url
   "https://raw.githubusercontent.com/spring-io/spring-javaformat/v0.0.6/.eclipse/eclipse-code-formatter.xml")
 '(lsp-java-vmargs
   (quote
	("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-javaagent:/home/matt/code/work/lombok-1.18.8.jar")))
 '(lsp-prefer-capf t)
 '(package-selected-packages
   (quote
	(lsp-treemacs lsp-java pkgbuild-mode dockerfile-mode magit yasnippet company which-key flycheck lsp-mode go-mode use-package)))
 '(tab-width 4)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(highlight ((t (:background "#454545" :foreground "#ffffff" :underline nil)))))
