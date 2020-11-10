;; Set font & font size
(set-face-attribute 'default nil :font "Fira Code Retina" :height 130)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 8)))

(use-package doom-themes
  :init (load-theme 'doom-horizon)
	(doom-themes-org-config)
	(doom-themes-treemacs-config))