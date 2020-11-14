;; --- PACKAGE SOURCES ---
;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(use-package exec-path-from-shell
	:init (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

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
 '(org-agenda-files
	 '("~/Dropbox/orgnzr/university/courses/content_management_systems/assignment4.org" "~/Dropbox/orgnzr/university/courses/impacts_of_future_technologies/analysis_of_the_now.org" "~/Dropbox/orgnzr/university/courses/impacts_of_future_technologies/envisioning_the_future.org" "~/Dropbox/orgnzr/in.org"))
 '(package-selected-packages
	 '(multiple-cursors magit exec-path-from-shell lsp-ivy lsp-mode company highlight-indent-guides add-node-modules-path prettier swiper prettier-js flycheck web-mode projectile ivy which-key neotree doom-themes doom-modeline use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; --- ACTUAL CONFIG BELOW ---

;; Default configuration before everything else
(load-file "./$HOME/.config/emacs/includes/defaults.el")

;; Keybindings
(load-file "./$HOME/.config/emacs/includes/keybindings.el")

;; Search & generic autocompletion
(load-file "./$HOME/.config/emacs/includes/completion.el")

;; Theming & Appearance
(load-file "./$HOME/.config/emacs/includes/appearance.el")

;; Sidebar / File Manager
(load-file "./$HOME/.config/emacs/includes/sidebar.el")

;; Window & Buffer management
(load-file "./$HOME/.config/emacs/includes/window.el")

;; Helpers
(load-file "./$HOME/.config/emacs/includes/helpers.el")

;; Project management
(load-file "./$HOME/.config/emacs/includes/project.el")

;; Git
(load-file "./$HOME/.config/emacs/includes/git.el")

;; Generic development
(load-file "./$HOME/.config/emacs/includes/dev-environment.el")

;; React development

;; Angular development

;; Org mode
(load-file "./$HOME/.config/emacs/includes/orgmode.el")
