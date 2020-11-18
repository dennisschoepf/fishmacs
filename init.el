(setq inhibit-startup-message t)
(setq visible-bell t)
(setq-default tab-width 2)

;; Default modes
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(delete-selection-mode 1)
(electric-pair-mode t)
(cua-mode t)
(show-paren-mode 1)

;; Disable line numbers for a few modes
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Important keybinding
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

;; Set font & font size
(set-face-attribute 'default nil :font "Fira Code Retina" :height 130)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 8)))

(use-package doom-themes
  :init (load-theme 'doom-horizon t)
	(doom-themes-org-config)
	(doom-themes-treemacs-config))

(windmove-default-keybindings 'meta)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(use-package neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :init
  (global-set-key [f8] 'neotree-toggle))

(use-package swiper)

(use-package ivy
	:after swiper
	:config
	(setq ivy-use-virtual-buffers t)
	(setq enable-recursive-minibuffers t)
	:init
	(ivy-mode 1)
	(global-set-key "\C-f" 'swiper))

(use-package company
  :config
  (setq company-idle-delay 0.1)
  (global-company-mode t))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package multiple-cursors
	:bind
	(("C-M-l" . mc/mark-all-like-this)
	 ("C-S-<mouse-1>" . 'mc/add-cursor-on-click)))

(use-package projectile
	:diminish projectile-mode
	:config (projectile-mode)
	:custom ((projectile-completion-system 'ivy))
	:bind-keymap
  ("C-c p" . projectile-command-map)
  :init
	(setq projectile-switch-project-action #'projectile-dired)
	(setq projectile-project-search-path '("~/Repositories/digitalh/" "~/Repositories/personal/" "~/Repositories/university/" "~/Repositories/util/" "~/Dropbox/" "/var/www/")))

(use-package magit
	:bind(("C-g" . magit-status)))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (lsp-mode . efs/lsp-mode-setup)
  (rjsx-mode . lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ivy)

(use-package rjsx-mode
  :mode "\\.js\\'"
  :config (setq js-indent-level 2))

(use-package olivetti
  :hook
  (org-mode . olivetti-mode)
  (text-mode . olivetti-mode))

;; Packages
(use-package org
  :hook
	(org-mode . flyspell-mode)
	(org-mode . auto-fill-mode)
	:bind
	(("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
	:config
	(setq org-agenda-files (list "~/Dropbox/orgnzr/_university.org" "~/Dropbox/orgnzr/_personal.org" "~/Dropbox/orgnzr/_work.org" "~/Dropbox/orgnzr/in.org" "~/Dropbox/orgnzr/tickler.org"))
	(setq org-ellipsis " ▾")
	(setq org-hide-leading-stars 't)
	(setq org-hide-emphasis-markers 't)
	(setq org-cycle-separator-lines 2)
	(setq org-startup-indented 't))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-indent
  :ensure nil
  :diminish
  :custom
  (org-indent-indentation-per-level 1))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-p") 'projectile-find-file)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-M-b") 'switch-to-buffer)
(global-set-key (kbd "<f6>") (lambda() (interactive)(find-file "~/.config/emacs/configuration.org")))
