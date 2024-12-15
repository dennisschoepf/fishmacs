(setq package-enable-at-startup nil)

;; Make the title bar transparent
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; And give the frame itself a little bit of opacity
(set-frame-parameter (selected-frame) 'alpha 96)
(add-to-list 'default-frame-alist '(alpha . 96))

;; And open emacs in fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(pixel-scroll-precision-mode 1)

(setq inhibit-splash-screen t
      use-file-dialog nil
      tab-bar-new-button-show nil
      tab-bar-close-button-show nil
			tab-bar-auto-width nil
      tab-line-close-button-show nil
			initial-scratch-message ";; Let's start ...\n")
