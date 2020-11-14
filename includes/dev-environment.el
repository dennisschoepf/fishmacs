(use-package web-mode
	:config
	(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.[s]css?\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
	(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
	(setq typescript-indent-level 2)
	(setq js-indent-level 2)
	(setq js2-indent-level 2)
	(setq javascript-indent-level 2)
	(setq js2-basic-offset 2)
	(setq web-mode-markup-indent-offset 2)
	(setq indent-tabs-mode nil)
	(setq web-mode-markup-indent-offset 2)
	(setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq react-indent-level 2)
	(setq web-mode-script-padding 2)
	:hook
	(web-mode . lsp-deferred)
	:init
	(add-hook 'web-mode-hook 'web-mode-init-hook))

(use-package add-node-modules-path
	:hook (web-mode))

(use-package highlight-indent-guides
	:hook (prog-mode web-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (highlight-indent-guides-mode t))

(use-package prettier
  :hook (web-mode)
  :config
  (global-prettier-mode t))

(use-package lsp-mode
	:hook
  ((web-mode . lsp-deferred)
	(lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred)

(use-package flycheck
  :hook (web-mode . flycheck))
