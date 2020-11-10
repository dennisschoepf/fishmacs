(use-package neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :init
  (global-set-key [f8] 'neotree-toggle))