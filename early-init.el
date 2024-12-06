(setq package-enable-at-startup nil)

;; Make the title bar transparent
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; And give the frame itself a little bit of opacity
(set-frame-parameter (selected-frame) 'alpha 96)
(add-to-list 'default-frame-alist '(alpha . 96))
