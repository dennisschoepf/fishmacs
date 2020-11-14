(use-package projectile
	:diminish projectile-mode
	:config (projectile-mode)
	:custom ((projectile-completion-system 'ivy))
	:bind-keymap
  ("C-c p" . projectile-command-map)
  :init
	(setq projectile-switch-project-action #'projectile-dired)
  (when (file-directory-p "~/")
		(setq projectile-project-search-path '("~/Repositories/digitalh/" "~/Repositories/personal/" "~/Repositories/university/" "~/Repositories/util/" "~/Dropbox/orgnzr/"))))
