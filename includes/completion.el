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
