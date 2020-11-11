(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package multiple-cursors
	:bind
	(("C-M-l" . mc/mark-all-like-this)
	 ("C-S-<mouse-1>" . 'mc/add-cursor-on-click)))
