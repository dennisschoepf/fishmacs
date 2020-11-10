;; --- PACKAGE SOURCES ---
;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("5036346b7b232c57f76e8fb72a9c0558174f87760113546d3a9838130f1cdb74" default))
 '(package-selected-packages
	 '(web-mode projectile ivy which-key neotree doom-themes doom-modeline use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; --- ACTUAL CONFIG BELOW ---

;; Default configuration before everything else
(load-file "./$HOME/.emacs.d/includes/defaults.el")

;; Keybindings
(load-file "./$HOME/.emacs.d/includes/keybindings.el")

;; Theming & Appearance
(load-file "./$HOME/.emacs.d/includes/appearance.el")

;; Sidebar / File Manager
(load-file "./$HOME/.emacs.d/includes/sidebar.el")

;; Window & Buffer management
(load-file "./$HOME/.emacs.d/includes/window.el")

;; Helpers
(load-file "./$HOME/.emacs.d/includes/helpers.el")

;; Search & generic autocompletion
(load-file "./$HOME/.emacs.d/includes/completion.el")

;; Project management
(load-file "./$HOME/.emacs.d/includes/project.el")

;; Generic development
(load-file "./$HOME/.emacs.d/includes/dev-environment.el")

;; React development

;; Angular development

;; Org mode
