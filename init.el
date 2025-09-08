;;;-*- lexical-binding: t; -*-
;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(setq user-full-name "Dennis Schoepf"
      user-mail-address "me@dnsc.io")

(defun start/org-babel-tangle-config ()
  "Automatically tangle our Emacs.org config file when we save it. Credit to Emacs From Scratch for this one!"
  (when (string-equal (file-name-directory (buffer-file-name))
					  (expand-file-name user-emacs-directory))
	;; Dynamic scoping to the rescue
	(let ((org-confirm-babel-evaluate nil))
	  (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'start/org-babel-tangle-config)))

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(elpaca-wait)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

(use-package emacs
  :ensure nil
  :custom
  ;; Set some global modes
  (global-visual-line-mode t)
  (delete-selection-mode t)
  (electric-pair-mode t)
  (global-auto-revert-mode t)
  (recentf-mode t)
  (visible-bell t)
  
  ;; Set some text editing defaults
	(electric-indent-mode t)
	(indent-tabs-mode nil)
  (tab-width 2)
  (fill-column 100)
  (display-line-numbers-type 'relative)
  (ring-bell-function 'ignore)
  
  ;; Configure scroll behavior
  (mouse-wheel-progressive-speed nil)
  (scroll-conservatively 10)
  (scroll-margin 8)
  
  ;; Undo behavior
  (undo-limit 67108864)
  (undo-strong-limit 100663296)
  (undo-outer-limit 1006632960)
  
  ;; Use encrypted authinfo file for auth-sources
  (auth-sources '("~/.authinfo.gpg"))

	;; keep backup and save files in a dedicated directory
	(create-lockfiles nil)
  (backup-directory-alist
          `((".*" . ,(concat user-emacs-directory "backups")))
          auto-save-file-name-transforms
          `((".*" ,(concat user-emacs-directory "backups") t)))
  
	;; Do not show native comp warning
	(native-comp-async-report-warnings-errors nil)
  :hook
  (prog-mode . display-line-numbers-mode)
  (prog-mode . hl-line-mode)
  (prog-mode . (lambda () (hs-minor-mode t)))
  :init
	;; Shorten those questions
	(defalias 'yes-or-no-p 'y-or-n-p)

  ;; MacOS specfic configuration
  (when (eq system-type 'darwin)
		(setq mac-option-modifier 'meta)
		(setq mac-right-option-modifier 'none)
		(setq insert-directory-program "/run/current-system/sw/bin/gls"))

  ;; Move customized variables to separate file
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)
  
  ;; File/folder navigation/operation settings
  (setq delete-by-moving-to-trash t)
  (file-name-shadow-mode 1)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  
	;; utf-8 everywhere
	(set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
          coding-system-for-read 'utf-8
          coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  :bind
  (([escape] . keyboard-escape-quit)))

(use-package dired
	:ensure nil
	:custom
	(dired-listing-switches "-lah --group-directories-first")
	(dired-dwim-target t)
	(dired-kill-when-opening-new-dired-buffer t)
  (wdired-allow-to-change-permissions t)
  (wdired-use-interactive-rename t)
  (wdired-confirm-overwrite t))

(use-package dired-narrow
	:ensure t
	:bind (:map dired-mode-map ("\C-s" . dired-narrow)))

(use-package dired-x
  :ensure nil
	:bind (:map dired-mode-map ("\C-h" . dired-omit-mode))
  :config
  (setq dired-omit-files
				(concat dired-omit-files "\\|^\\..+$"))
	:hook
	(dired-mode . dired-omit-mode))

(use-package all-the-icons
	:ensure t)

(use-package all-the-icons-dired
	:ensure t
	:hook
	(dired-mode . all-the-icons-dired-mode))

(use-package diredfl
	:ensure t
	:hook
	(dired-mode . diredfl-mode))

(use-package undo-fu
  :ensure t
  :custom
  (undo-fu-allow-undo-in-region t))

(use-package undo-fu-session
  :ensure t
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
	(undo-fu-session-global-mode t))

(use-package vundo
  :ensure t
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package evil
  :ensure t
  :init ;; Execute code Before a package is loaded
  (evil-mode)
  (setq evil-want-C-i-jump nil)
  :config ;; Execute code After a package is loaded
  (evil-set-initial-state 'eat-mode 'insert) ;; Set initial state in eat terminal to insert mode
	(evil-set-initial-state 'magit-diff-mode 'insert)
  :custom ;; Customization of package custom variables
  (evil-want-keybinding nil)    ;; Disable evil bindings in other modes (It's not consistent and not good)
  (evil-want-C-u-scroll t)      ;; Set C-u to scroll up
  (evil-want-C-i-jump nil)      ;; Disables C-i jump
  (evil-undo-system 'undo-fu) 
  (org-return-follows-link t)   ;; Sets RETURN key in org-mode to follow links
  :bind (:map evil-motion-state-map
			  ("SPC" . nil)
			  ("RET" . nil)
			  ("TAB" . nil)))

(use-package evil-collection
  :after evil
  :config
  ;; Setting where to use evil-collection
  (setq evil-collection-mode-list '(dired ibuffer magit corfu vertico consult))
  (evil-collection-init))

(use-package evil-goggles
  :after evil
  :config
  (setq evil-goggles-duration 0.100)
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(defun dnsc/dired-open-to-side ()
  "Opens dired at the current directory in a window to the side"
  (interactive)
  (split-window-horizontally)
  (windmove-right)
  (dired-jump))

(defun dnsc/open-agenda-only-window ()
  "Opens agenda in full-screen (only-window)"
  (interactive)
  (let (current-value org-agenda-window-setup)
		(unwind-protect
				(progn
					(setq org-agenda-window-setup 'only-window)
					(org-agenda nil "p"))
			(setq org-agenda-window-setup current-value))))

(use-package general
	:ensure t
	:demand t
  :config
  (general-evil-setup)
  ;; Set up 'SPC' as the leader key
  (general-create-definer start/leader-keys
		:states '(normal insert visual motion emacs)
		:keymaps 'override
		:prefix "SPC"           ;; Set leader key
		:global-prefix "C-SPC") ;; Set global leader key

  (start/leader-keys
		"." '(find-file :wk "Find file"))

  (start/leader-keys
		"SPC" '(project-find-file :wk "[f]ind a file in the project"))

  (start/leader-keys
		"TAB" '(activities-switch  :wk "Switch activity"))

  (start/leader-keys
		"-" '(dired-jump :wk "open dired at current directory"))

  (start/leader-keys
		"a" '(:ignore t :wk "[a]ctivities")
    "a n" '(activities-new :wk "[n]ew activity")
    "a c" '(activities-define :wk "[c]reate activity from current tab layout")
    "a r" '(activities-resume :wk "[r]esume activity")
    "a l" '(activities-list :wk "[l]ist activities")
    "a s" '(activities-suspend :wk "[s]uspend activity")
    "a b" '(activities-switch-buffer :wk "switch activity [b]uffer")
    "a R" '(activities-revert :wk "[R]evert activity to default state")
    "a d" '(activities-kill :wk "[d]elete activity"))

  (start/leader-keys
		"f" '(:ignore t :wk "[f]ind")
		"f c" '((lambda () (interactive) (find-file "~/.emacs.d/README.org")) :wk "Edit emacs [c]onfig")
		"f s" '(save-buffer :wk "Saves current buffer")
		"f r" '(consult-recent-file :wk "Find [r]ecent files")
		"f f" '(consult-fd :wk "Find [f]iles")
		"f h" '(consult-org-heading :wk "Find [h]eadline")
		"f y" '(consult-yank-from-kill-ring :wk "[y]ank from kill ring")
		"f g" '(consult-ripgrep :wk "Find with rip[g]rep")
		"f l" '(consult-goto-line :wk "Find [l]ine")
		"f i" '(consult-imenu :wk "Find [i]menu buffer locations"))

  (start/leader-keys
		"b" '(:ignore t :wk "[b]uffers")
		"b b" '(consult-buffer :wk "switch to [b]uffer")
		"b d" '(kill-current-buffer :wk "[d]elete this buffer")
		"b n" '(next-buffer :wk "[n]ext buffer")
		"b p" '(previous-buffer :wk "[p]revious buffer")
		"b r" '(revert-buffer :wk "[r]eload buffer"))

  (start/leader-keys
		"w" '(:ignore t :wk "[w]indow")
		"w s" '(split-window-vertically :wk "[s]plit window horizontally")
		"w v" '(split-window-horizontally :wk "Split window [v]ertically")
		"w d" '(delete-window :wk "[d]elete window")
		"w z" '(delete-other-windows :wk "[z]oom to window")
		"w x" '(kill-buffer-and-window :wk "E[x]terminate buffer and window")
		"w h" '(windmove-left :wk "Move to left window")
		"w k" '(windmove-up :wk "Move to upper window")
		"w j" '(windmove-down :wk "Move to lower window")
		"w l" '(windmove-right :wk "Move to right window"))

  (start/leader-keys
		"n" '(:ignore t :wk "[n]otes")
		"n n" '(denote :wk "[n]ew denote")
		"n b" '(denote-backlinks :wk "show [b]acklinks for current denote")
		"n r" '(denote-rename-file :wk "[r]ename current denote")
		"n i" '(denote-link-or-create :wk "[i]nsert link to existing denote or create new")
		"n f" '(consult-denote-find :wk "[f]ind denote")
		"n s" '(consult-denote-grep :wk "[s]earch in denotes")
		"n l" '(denote-menu-list-notes :wk "[l]ist denotes"))

  (start/leader-keys
		"j" '(:ignore t :wk "popups")
		"j j" '(popper-toggle :wk "Open popup")
		"j c" '(popper-cycle :wk "Cycle popups")
		"j t" '(popper-toggle-type :wk "Convert current buffer to popup buffer"))

  (start/leader-keys
		"d" '(:ignore t :wk "AI")
		"d s" '(gptel-send :wk "Send to gptel")
		"d c" '(gptel-send :wk "Open gptel chat")
		"d m" '(gptel-menu :wk "Open gptel menu"))

  (start/leader-keys
		"o" '(:ignore t :wk "[o]rg/[o]pen")
		"o d" '(dired :wk "Open [d]ired")
		"o l" '(org-agenda :wk "Open al[l] agenda views")
		"o a" '((lambda () (interactive) (org-agenda nil "p")) :wk "Open personal [a]genda")
		"o w a" '((lambda () (interactive) (org-agenda nil "w")) :wk "Open work [a]genda")
		"o w f" '(dnsc/open-agenda-only-window :wk "Open personal [a]genda")
		"o w n" '((lambda () (interactive) (find-file "~/orgnzr/work.org")) :wk "Open work [n]ote")
		"o m t" '(org-todo :wk "Change todo state")
		"o m c" '(org-toggle-checkbox :wk "Toggle [c]heckbox")
		"o c" '(org-capture :wk "[o]rg-[c]apture a new task"))

  (start/leader-keys
		"g" '(:ignore t :wk "[g]it & more")
		"g b" '(magit-branch-checkout :wk "Switch [b]ranch")
		"g c" '(:ignore t :wk "[c]reate")
		"g c b" '(magit-branch-and-checkout :wk "[b]ranch and checkout")
		"g c c" '(magit-commit-create :wk "[c]ommit")
		"g f" '(:ignore t :wk "[f]ind")
		"g f c" '(magit-show-commit :wk "[c]ommit")
		"g f l" '(magit-log :wk "[l]og")
		"g f f" '(magit-find-file :wk "[f]ile")
		"g F" '(magit-fetch :wk "[F]etch")
		"g l" '(git-link :wk "Navigate to git forge [l]ink")
		"g t" '(git-timemachine :wk "Navigate to git forge [l]ink")
		"g s" '(magit-stash :wk "[s]tash")
		"g g" '(magit-status :wk "Ma[g]it status"))

  (start/leader-keys
		"h" '(:ignore t :wk "[h]elp")
		"h s" '(describe-symbol :wk "Get help for [s]ymbol")
		"h k" '(describe-key :wk "Get help for [s]ymbol")
		"h v" '(describe-variable :wk "Get help for [v]ariable")
		"h f" '(describe-function :wk "Get help for [f]unction")
		"h r r" '((lambda () (interactive) (load-file user-init-file)) :wk "Reload Emacs config"))

  (start/leader-keys
		"p" '(:ignore t :wk "[p]rojects")
		"p p" '(project-switch-project :wk "switch [p]rojects")
		"p b" '(consult-project-buffer :wk "switch [b]uffers within project")
		"p g" '(consult-ripgrep :wk "[s]earch within project")
		"p s" '(project-shell :wk "Opeommitn [s]hell within project")
		"p d" '(project-dired :wk "Open [d]ired in project root")
		"p c" '(project-compile :wk "[c]ompile project")
		"p k" '(project-kill-buffers :wk "[d]elete all project buffers")
		"p r" '(project-query-replace-regexp :wk "[r]eplace in current project")
		"p x" '(project-async-shell-command :wk "e[x]ecute shell command"))
  
  (start/leader-keys
		"s" '(:ignore t :wk "[s]earch/[s]pell")
		"s c" '(jinx-correct :wk "[c]orrect spelling")
		"s l" '(jinx-languages :wk "Jinx [l]anguages"))

  (start/leader-keys
		"t" '(:ignore t :wk "[t]abs")
		"t d" '(tab-close :wk "[d]elete tab")
		"t n" '(tab-next :wk "[n]ext tab")
		"t p" '(tab-previous :wk "[p]revious tab")
		"t t" '(tab-switch :wk "[s]witch tabs"))

  (start/leader-keys
		"q" '(:ignore t :wk "[q]uit")
		"q q" '(kill-emacs :wk "[q][q]uit Emacs and Daemon")))
(elpaca-wait)

(use-package which-key
  :diminish
  :init
  (which-key-mode 1)
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha) ;; Same as default, except single characters are sorted alphabetically
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 4) ;; Number of spaces to add to the left of each column
  (which-key-min-display-lines 6)  ;; Increase the minimum lines to display, because the default is only 1
  (which-key-idle-delay 0.5)       ;; Set the time delay (in seconds) for the which-key popup to appear
  (which-key-max-description-length 35)
  (which-key-allow-imprecise-window-fit nil))

(use-package avy
  :ensure t
  :after evil
  :custom
  (avy-background nil)
  (avy-all-windows nil)
  :bind
  (:map evil-normal-state-map ("RET" . avy-goto-char-2)))

(use-package evil-replace-with-register
  :ensure t
  :bind
  (:map evil-normal-state-map ("s" . evil-replace-with-register))
  (:map evil-visual-state-map ("s" . evil-replace-with-register)))

(use-package activities
  :ensure t
  :custom
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)
  :init
  (activities-mode)
  (activities-tabs-mode))

(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers '(".project" "go.mod" "package.json" "deps.edn" "bb.edn")))

(use-package popper
  :ensure t
  :init
  (setq popper-group-function #'popper-group-by-directory)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package modus-themes
	:ensure t
  :config

  (custom-set-faces
   '(tab-bar ((t (:height 0.85))))
   '(activities-tabs
     ((t (:slant italic :foreground "#c6daff" :background "#0d0e1c"))))
   '(tab-bar-tab-inactive
     ((t (:slant italic :foreground "#c6daff" :background "#0d0e1c")))))
  
  (setq modus-themes-common-palette-overrides
		'((border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)
		  (fg-heading-1 blue-cooler)
		  (prose-done fg-dim)
		  (prose-done fg-dim)
		  (fringe unspecified)
		  (bg-line-number-inactive unspecified)
          (bg-line-number-active bg-dim)
          (bg-hl-line bg-dim)
		  (bg-prose-block-delimiter unspecified)
		  (bg-tab-bar bg-main)
		  (bg-tab-current bg-main)
		  (fg-tab-current bg-main)
		  (bg-tab-other bg-main)
		  (fg-tab-other bg-main)
		  (comment fg-dim)))

  (setq modus-themes-fringes nil)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-custom-auto-reload t)

  (load-theme 'modus-vivendi-tinted))

(use-package evil-anzu)

(use-package anzu
  :init
  (global-anzu-mode +1))

(use-package mood-line
	:ensure t
	:config
	(mood-line-mode)
  :custom
	(mood-line-format (mood-line-defformat
										 :left
                     (((when (mode-line-window-selected-p)
                         (mood-line-segment-modal)) . " ")
                      ((when (mode-line-window-selected-p)
                         (propertize "|" 'face 'modus-themes-fg-cyan-faint)) . " ")
											((mood-line-segment-buffer-status) . " ")
											((if (mode-line-window-selected-p)
													 (mood-line-segment-buffer-name)
												 (propertize (mood-line-segment-buffer-name) 'face 'mood-line-unimportant)) . "")
                      ((propertize " (" 'face 'mood-line-unimportant) . "")
                      ((propertize (mood-line-segment-major-mode) 'face 'mood-line-unimportant) . "")
                      ((propertize ")" 'face 'mood-line-unimportant) . "")
                      ((when (mode-line-window-selected-p)
                         (propertize " |" 'face 'modus-themes-fg-cyan-faint)) . " ")
                      ((when (mode-line-window-selected-p)
                         (mood-line-segment-vc)) . " "))
                     :right
                     (((when (mode-line-window-selected-p)
                         (mood-line-segment-checker)) . " ")
											;; ((when (mode-line-window-selected-p)
                      ;;    (mood-line-segment-process)) . " ")
                      ((mood-line-segment-anzu) . " ")
                      ((when (mode-line-window-selected-p)
                         (mood-line-segment-cursor-position)) . " ")
                      ((when (mode-line-window-selected-p)
                         (mood-line-segment-scroll)) . " ")
                      (propertize "[" 'face 'modus-themes-fg-magenta-intense)
											((propertize (mood-line-segment-project) 'face 'modus-themes-fg-magenta-intense) . "")
                      (propertize "]" 'face 'modus-themes-fg-magenta-intense)))))

(set-face-attribute 'default nil
					:font "VictorMono Nerd Font"
					:height 180
					:weight 'normal)
(set-face-attribute 'variable-pitch nil
					:font "VictorMono Nerd Font"
					:height 180
					:weight 'normal)
(set-face-attribute 'fixed-pitch nil
					:font "VictorMono Nerd Font"
					:height 180
					:weight 'normal)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
					:slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
					:slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "VictorMono Nerd Font-18"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package emacs
  :ensure nil
  :bind
  ("C-+" . text-scale-increase)
  ("C--" . text-scale-decrease)
  ("<C-wheel-up>" . text-scale-increase)
  ("<C-wheel-down>" . text-scale-decrease))

(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package vertico
  :init
  (vertico-mode))

(savehist-mode) ;; Enables save history mode

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  (corfu-quit-no-match nil)
	(corfu-auto t)
  (corfu-quit-no-match t)
	(corfu-auto-prefix 2)
  (corfu-popupinfo-mode t)
  (corfu-popupinfo-delay 0.5)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
	:ensure t
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package cape
	:ensure t
  :bind ("C-c p" . cape-prefix-map)
	:init
	(add-hook 'completion-at-point-functions #'cape-dabbrev)
	(add-hook 'completion-at-point-functions #'cape-file)
	(add-hook 'completion-at-point-functions #'cape-keyword)
	(add-hook 'completion-at-point-functions #'cape-elisp-symbol)
	(add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;; consult-theme :preview-key '(:debounce 0.2 any)
  ;; consult-ripgrep consult-git-grep consult-grep
  ;; consult-bookmark consult-recent-file consult-xref
  ;; consult--source-bookmark consult--source-file-register
  ;; consult--source-recent-file consult--source-project-recent-file
  ;; :preview-key "M-."
  ;; :preview-key '(:debounce 0.4 any))

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
   ;;;; 1. project.el (the default)
  ;;(setq consult-project-function #'consult--default-project--function)
   ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
   ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
   ;;;; 4. projectile.el (projectile-project-root)
  ;;(autoload 'projectile-project-root "projectile")
  ;;(setq consult-project-function (lambda (_) (projectile-project-root)))
   ;;;; 5. No project support
  ;;(setq consult-project-function nil)
  )

(use-package diff-hl
  :ensure t
  :custom
  (diff-hl-draw-borders nil)
  :hook ((dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode))

(use-package magit
  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-repository-directories (list (cons elpaca-repos-directory 1)))
  (magit-diff-refine-hunk 'all)
  :config
  (transient-bind-q-to-quit))
(use-package transient :defer t) 
(use-package forge
  :after magit
  :init (setq forge-add-default-bindings nil
              forge-display-in-status-buffer nil
              forge-add-pullreq-refspec nil))

(use-package git-link
  :custom
  (git-link-open-in-browser t))

(use-package git-timemachine
	:ensure t
	:hook (evil-normalize-keymaps . git-timemachine-hook)
	:config
	(evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
	(evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision))

(use-package org
  :ensure nil
  :custom
  (org-directory "~/orgnzr/")
  ;; Fixing source block indentation
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  (org-edit-src-preserve-indentation nil)
  ;; (org-log-done 'time)
  (org-archive-location "~/orgnzr/archive.org::* From %s")
  (org-startup-folded t)
  (org-startup-indented t)
  (org-todo-keywords
   '((sequence "PROJECT(p)" "TODO(t)" "NEXT(n)" "|" "DONE(d)")))
  (org-default-notes-file "~/orgnzr/inbox.org")
  (org-agenda-files '("~/orgnzr/"))
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-refile-targets
   '((nil :maxlevel . 5)
     (org-agenda-files :maxlevel . 5)))
  (org-capture-templates
   '(("t" "Task" entry (file "~/orgnzr/inbox.org")
      "* TODO %?\n %i\n")
     ("l" "Task  line" entry (file "~/orgnzr/inbox.org")
      "* TODO %?\n Relevant line: [[file://%F::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (line-number-at-pos)))]]\n")
     ("w" "Work Task" entry (file+olp+datetree "~/orgnzr/work.org")
      "* TODO %?\n")
     ("n" "New note" plain
      (file denote-last-path)
      #'denote-org-capture
      :no-save t
      :immediate-finish nil
      :kill-buffer t
      :jump-to-captured t)))
  (org-agenda-custom-commands
   '(("p" "Personal" 
	    ((agenda "")
	     (todo "NEXT" ((org-agenda-overriding-header "Next Tasks")))
       (alltodo "" (
                   (org-agenda-files '("~/orgnzr/inbox.org"))
                   (org-agenda-overriding-header "Uncategorized")))))))
  :hook
  (org-mode . org-indent-mode)
  (org-mode . (lambda() (electric-indent-local-mode -1)))
  (org-mode . (lambda ()
                (setq-local electric-pair-inhibit-predicate
                            `(lambda (c)
                               (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))

(use-package org-modern
  :ensure t
  :custom
  (org-modern-star 'replace)
  :hook 
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

(use-package visual-fill-column
  :ensure t
  :custom
  (visual-fill-column-center-text t)
  (visual-fill-column-enable-sensible-window-split t)
  (visual-fill-column-fringes-outside-margins t)
  :hook
  (org-mode . visual-fill-column-mode))

(use-package org-tempo
  :ensure nil
  :after org)

(use-package denote
  :ensure t
  :custom
  (denote-directory (expand-file-name "~/orgnzr"))
  (denote-save-buffers nil)
  (denote-known-keywords '("dev" "ux" "design" "list" "fleeting" "meeting"))
  (denote-infer-keywords t) 
  (denote-sort-keywords t) 
  (denote-date-prompt-use-org-read-date t)
  (denote-dired-directories
      (list denote-directory
            (thread-last denote-directory (expand-file-name "attachments"))))
  :hook
  (dired-mode . denote-dired-mode)
  :config
  (denote-rename-buffer-mode 1))

(use-package consult-denote
  :ensure t
  :after denote
  :config
  (consult-denote-mode 1))

(use-package denote-menu
  :ensure t
  :after denote
  :general
	(:states 'normal
					 :keymaps 'denote-menu-mode-map
					 "r" 'denote-menu-filter
					 "q" 'kill-current-buffer
					 "o" 'denote-menu-filter-out-keyword
					 "y" 'denote-menu-filter-by-keyword
					 "d" 'denote-menu-export-to-dired
					 "c" 'denote-menu-clear-filters
					 "k" nil))

(use-package org-alert
   :ensure t 
   :custom
   (org-alert-notification-title "Orgnzr")
   (org-alert-interval 600)
   (org-alert-notify-cutoff 5)
   (org-alert-notify-after-event-cutoff 10) 
   :config 
   (org-alert-enable))

(use-package alert 
   :ensure t 
   :config (setq alert-default-style 'osx-notifier))

(use-package jinx
  :ensure t)

(use-package diminish)

(setq elpaca-after-init-time (or elpaca-after-init-time (current-time)))
(elpaca-wait)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb
