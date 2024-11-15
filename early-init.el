(setq package-enable-at-startup nil)

;; Transparent background
;; (set-frame-parameter nil 'alpha-background 80)
;; (add-to-list 'default-frame-alist '(alpha-background . 80))
(set-frame-parameter (selected-frame) 'alpha 97)
(add-to-list 'default-frame-alist '(alpha . 97))
