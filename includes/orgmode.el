;; Functions
(defun dnsc/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; Packages
(use-package org
	:bind
	(("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
	:config
	(setq org-agenda-files (directory-files-recursively "~/Dropbox/orgnzr" "\\.org$"))
	(setq org-ellipsis " ▾")
	:init
	(add-hook 'after-save-hook 'org-icalendar-combine-agenda-files))

(use-package visual-fill-column
  :hook (org-mode . dnsc/org-mode-visual-fill))
