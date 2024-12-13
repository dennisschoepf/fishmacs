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

(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
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
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;;When installing a package which modifies a form used at the top-level
;;(e.g. a package which adds a use-package key word),
;;use `elpaca-wait' to block until that package has been installed/configured.
;;For example:
;;(use-package general :ensure t :demand t)
;;(elpaca-wait)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

(use-package evil
  :init ;; Execute code Before a package is loaded
  (evil-mode)
  (setq evil-want-C-i-jump nil)
  :config ;; Execute code After a package is loaded
  (evil-set-initial-state 'eat-mode 'insert) ;; Set initial state in eat terminal to insert mode
  :custom ;; Customization of package custom variables
  (evil-want-keybinding nil)    ;; Disable evil bindings in other modes (It's not consistent and not good)
  (evil-want-C-u-scroll t)      ;; Set C-u to scroll up
  (evil-want-C-i-jump nil)      ;; Disables C-i jump
  (evil-undo-system 'undo-fu) 
  (org-return-follows-link t)   ;; Sets RETURN key in org-mode to follow links
  ;; Unmap keys in 'evil-maps. If not done, org-return-follows-link will not work
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
  (setq evil-goggles-enable-delete nil)
  (setq evil-goggles-enable-paste nil)
  (setq evil-goggles-enable-change nil)
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
					(org-agenda nil "w"))
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
		"TAB" '(tabspaces-open-or-create-project-and-workspace  :wk "Open or create workspace with project"))

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
		"w h" '(windmove-left :wk "Move to left window")
		"w k" '(windmove-up :wk "Move to upper window")
		"w j" '(windmove-down :wk "Move to lower window")
		"w l" '(windmove-right :wk "Move to right window")
		"w z" '(zoom-window-zoom :wk "Toggle [z]oom for current window"))

  (start/leader-keys
		"n" '(dired-jump :wk "ope[n] dired at current directory"))

  (start/leader-keys
		"B" '(tabspaces-switch-buffer-and-tab :wk "Show all [B]uffers and switch to project and buffer"))

  (start/leader-keys
		"o" '(:ignore t :wk "[o]rg")
		"o l" '(org-agenda :wk "Open al[l] agenda views")
		"o a" '((lambda () (interactive) (org-agenda nil "p")) :wk "Open personal [a]genda")
		"o w a" '((lambda () (interactive) (org-agenda nil "w")) :wk "Open work [a]genda")
		"o w f" '(dnsc/open-agenda-only-window :wk "Open work [a]genda")
		"o w n" '((lambda () (interactive) (find-file "~/orgnzr/work.org")) :wk "Open work [n]ote")
		"o n n" '(org-roam-node-find :wk "Open roam note")
		"o n i" '(org-roam-node-insert :wk "Insert roam note")
		"o n t" '(org-roam-buffer-toggle :wk "Toggle roam buffer")
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
		"g f f" '(magit-find-file :wk "[f]ile")
		"g F" '(magit-fetch :wk "[F]etch")
		"g l" '(git-link :wk "Navigate to git forge [l]ink")
		"g t" '(git-timemachine :wk "Navigate to git forge [l]ink")
		"g s" '(magit-stash :wk "[s]tash")
		"g g" '(magit-status :wk "Ma[g]it status"))

  (start/leader-keys
		"h" '(:ignore t :wk "[h]elp") ;; To get more help use C-h commands (describe variable, function, etc.)
		"h s" '(describe-symbol :wk "Get help for [s]ymbol")
		"h k" '(describe-key :wk "Get help for [s]ymbol")
		"h v" '(describe-variable :wk "Get help for [v]ariable")
		"h f" '(describe-function :wk "Get help for [f]unction")
		"h r r" '((lambda () (interactive) (load-file user-init-file)) :wk "Reload Emacs config"))

  (start/leader-keys
		"p" '(:ignore t :wk "[p]rojects") ;; To get more help use C-h commands (describe variable, function, etc.)
		"p g" '(consult-ripgrep :wk "[s]earch within project") ;; Maybe use something else here
		"p s" '(project-shell :wk "Open [s]hell within project")
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
		"t" '(:ignore t :wk "[t]abspaces")
		"t s" '(tabspaces-save-session :wk "[s]ave session")
		"t r" '(tabspaces-restore-session :wk "[r]estore session")
		"t d" '(tabspaces-close-workspace :wk "[d]elete tabspace")
		"t D" '(tabspaces-clear-buffers :wk "[D]elete tabspace except current buffer")
		"t x" '(tabspaces-kill-buffers-close-workspace :wk "Delete tabspace and clear all open buffers"))

  (start/leader-keys
		"q" '(:ignore t :wk "[q]uit")
		"q q" '(kill-emacs :wk "[q][q]uit Emacs and Daemon")))
;; Ensure that the :general keyword is available
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

(fset 'yes-or-no-p 'y-or-n-p)

(use-package emacs
  :ensure nil
  :custom
  ;; Disable unwanted elements
  (menu-bar-mode nil)
  (scroll-bar-mode nil)
  (tool-bar-mode nil)
  (inhibit-startup-screen t)
  (ring-bell-function 'ignore)
  (blink-cursor-mode nil)
  
  ;; Configure the tab bar to work well with tabspaces.el
  (tab-bar-mode nil)
  (tab-bar-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-auto-width nil)
	
	;; Set scratch buffer message
	(initial-scratch-message ";; Let's start ...\n")

  ;; Set some global modes
  (global-visual-line-mode t)
  (delete-selection-mode t)
  (electric-pair-mode t)
  (global-auto-revert-mode t)
  (recentf-mode t)
  (visible-bell t)
	(pixel-scroll-precision-mode t)
  
  ;; Set some text editing defaults
	(electric-indent-mode t)
  (tab-width 2)
  (fill-column 100)
  (display-line-numbers-type 'relative)
  
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
  
	;; Do not show native comp warning
	(native-comp-async-report-warnings-errors nil)
  :hook
  (prog-mode . display-line-numbers-mode)
  (prog-mode . hl-line-mode)
  (prog-mode . (lambda () (hs-minor-mode t)))
  :init
  ;; MacOS specfic configuration
  (when (eq system-type 'darwin)
		(setq mac-right-option-modifier "none")
		(setq insert-directory-program "/opt/homebrew/bin/gls"))

  ;; Move customized variables to separate file
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)
  :bind
  (([escape] . keyboard-escape-quit)))

(use-package emacs
  :ensure nil
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-use-interactive-rename t)
  (wdired-confirm-overwrite t))

(use-package dired
	:ensure nil
	:custom
	(dired-listing-switches "-lah --group-directories-first")
	(dired-dwim-target t)
	(dired-kill-when-opening-new-dired-buffer t))

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

(use-package zoom-window
  :ensure t
  :custom
  (zoom-window-mode-line-color "DarkSlateGray"))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))

;; consult-buffer only shows workspace buffers unless 'b' is pressed
(with-eval-after-load 'consult
(consult-customize consult--source-buffer :hidden t :default nil)
(defvar consult--source-workspace
  (list :name     "Workspace Buffers"
        :narrow   ?w
        :history  'buffer-name-history
        :category 'buffer
        :state    #'consult--buffer-state
        :default  t
        :items    (lambda () (consult--buffer-query
                         :predicate #'tabspaces--local-buffer-p
                         :sort 'visibility
                         :as #'buffer-name)))

  "Set workspace buffer list for consult-buffer.")
(add-to-list 'consult-buffer-sources 'consult--source-workspace))

(use-package tabspaces
  :ensure (:host github :repo "mclear-tools/tabspaces")
  :hook (after-init . tabspaces-mode)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "main")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-session t)
  (tab-bar-new-tab-choice "*scratch*")
	(tab-bar-mode nil))

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
											((when (mode-line-window-selected-p)
                         (mood-line-segment-process)) . " ")
                      ((mood-line-segment-anzu) . " ")
                      ((when (mode-line-window-selected-p)
                         (mood-line-segment-cursor-position)) . " ")
                      ((when (mode-line-window-selected-p)
                         (mood-line-segment-scroll)) . " ")
                      (propertize "[" 'face 'modus-themes-fg-magenta-intense)
											((propertize (mood-line-segment-project) 'face 'modus-themes-fg-magenta-intense) . "")
                      (propertize "]" 'face 'modus-themes-fg-magenta-intense)))))

(use-package project
  :ensure nil
  :custom
  (project-vc-ignores '("target/" "bin/" "out/" "node_modules/"))
  (project-vc-extra-root-markers '(".project" "package.json" "Cargo.toml" "go.mod" "Gemfile")))

(use-package modus-themes
	:ensure t
  :config

  (custom-set-faces
   '(tab-bar ((t (:height 0.85))))
   '(tab-bar-tab-inactive
     ((t (:slant italic :foreground "#606270")))))
  
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
		  (bg-tab-other bg-main)
		  (comment fg-dim)))

  (setq modus-themes-fringes nil)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-custom-auto-reload t)

  (load-theme 'modus-vivendi-tinted))

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
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

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
  (corfu-quit-at-boundary nil)
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
  (setq consult-project-function nil)
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
  (org-log-done 'time)
  (org-startup-folded t)
  (org-startup-indented t)
  (org-todo-keywords
   '((sequence "PROJECT(p)" "TODO(t)" "NEXT(n)" "|" "DONE(d)")))
  (org-default-notes-file "~/orgnzr/inbox.org")
  (org-agenda-files '("~/orgnzr"))
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
    "* TODO %?\n")))
  (org-agenda-custom-commands
   '(("p" "Personal" 
	  ((agenda "")
	  (todo "NEXT" ((org-agenda-overriding-header "Next Tasks")))
		(tags "+inbox" ((org-agenda-overriding-header "Uncategorized"))))
	  ((org-agenda-tag-filter-preset '("-work"))))
	 ("w" "Work"
	  ((agenda "")
	  (tags "+work+TODO=\"NEXT\"" ((org-agenda-overriding-header "Time-Insensitive Tasks")))
	  (tags "+work+TODO=\"TODO\""
			((org-agenda-overriding-header "Unscheduled Tasks")
			 (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp)))))
	  ((org-agenda-tag-filter-preset '("+work"))))))
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

(use-package org-drill
  :ensure t)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/orgnzr/notes")
  (org-roam-completion-everywhere t)
  :config
  (org-roam-setup))

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

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package lsp-mode
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode
					 astro-ts-mode) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-completion-provider :none)       ; Using Corfu as the provider
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)
  (lsp-keep-workspace-alive nil)
  (lsp-idle-delay 0.1)
  (lsp-enable-xref t)
  (lsp-auto-configure nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-enable-dap-auto-configure nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-imenu nil)
  (lsp-enable-indentation nil)
  (lsp-enable-links nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-suggest-server-download t)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-diagnostic-max-lines 20)
	(lsp-ui-sideline-show-diagnostics nil)
	(lsp-ui-sideline-show-code-actions nil)
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit nil)
  (lsp-completion-show-kind t)
	(lsp-completion-show-detail nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-signature-doc-lines 1)
	(lsp-signature-auto-activate nil)
	(lsp-signature-render-documentation nil)
  (lsp-ui-doc-use-childframe t)
  (lsp-eldoc-render-all nil)
  (lsp-lens-enable nil)
  (lsp-semantic-tokens-enable nil)
  :init
  (setq lsp-use-plists t)
	:config
	(add-to-list 'lsp-language-id-configuration '(".*\\.astro" . "astro")))

(use-package lsp-completion
  :ensure nil
  :hook ((lsp-mode . lsp-completion-mode)))

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(use-package clojure-ts-mode
	:ensure t
	:custom
	(clojure-ts-comment-macro-font-lock-body t))

(use-package cider
  :ensure t
	:custom
	(nrepl-hide-special-buffers t)
  (nrepl-log-messages nil)
  (cider-font-lock-dynamically '(macro core function var deprecated))
  (cider-overlays-use-font-lock t)
  (cider-print-options '(("length" 100)))
  (cider-prompt-for-symbol nil)
	(cider-repl-display-in-current-window t)
  (cider-repl-history-display-duplicates nil)
  (cider-repl-history-display-style 'one-line)
  (cider-repl-history-highlight-current-entry t)
  (cider-repl-history-quit-action 'delete-and-restore)
  (cider-repl-history-highlight-inserted-item t)
  (cider-repl-history-size 1000)
  (cider-repl-result-prefix ";; => ")
  (cider-repl-use-clojure-font-lock t)
  (cider-repl-use-pretty-printing t)
  (cider-repl-wrap-history nil)
	(cider-repl-display-help-banner nil)
	(cider-show-error-buffer 'except-in-repl)
  (cider-stacktrace-default-filters '(tooling dup))
  (cider-repl-pop-to-buffer-on-connect 'display-only)
  :init
	(defun dnsc/start-babashka-repl-server ()
		(interactive)
		(let ((default-directory (project-root (project-current))))
      (start-process "Babashka nrepl-server" "bb-nrepl" "bb" "nrepl-server")))

  (defun dnsc/connect-to-cider-repl-on-the-side ()
    (interactive)
		(split-window-horizontally 90)
		(cider-connect-clj '(:host "localhost" :port 1667))
		(windmove-right))
	
	(defun dnsc/connect-and-open-bb-nrepl-server ()
		(interactive)
		(dnsc/start-babashka-repl-server)
		(sleep-for 2)
		(dnsc/connect-to-cider-repl-on-the-side))
  :general
	(:states 'normal
					 :keymaps 'cider-mode-map
					 "gd" 'cider-find-var
					 "gb" 'cider-pop-back
					 "gr" 'cider-xref-fn-refs-select
					 "SPC lr" 'cider-jack-in
					 "SPC lc" 'dnsc/connect-to-cider-repl-on-the-side
					 "SPC lb" 'cider-load-buffer
					 "SPC rb" 'dnsc/connect-and-open-bb-nrepl-server
					 "SPC rn" 'cider-repl-set-ns
					 "SPC ef" 'cider-eval-defun-at-point
					 "SPC ee" 'cider-eval-last-sexp
					 "SPC en" 'cider-ns-refresh
					 "SPC etr" 'cider-test-run-test
					 "SPC etn" 'cider-test-run-ns-tests
					 "SPC etp" 'cider-test-run-project-tests
					 "SPC etf" 'cider-test-rerun-failed-tests
					 "SPC etr" 'cider-test-show-report
					 "SPC da" 'cider-apropos
					 "SPC dd" 'cider-doc
					 "SPC dc" 'cider-clojuredocs
					 "SPC dj" 'cider-javadoc)
	:hook
	(clojure-ts-mode . cider-mode))

(use-package astro-ts-mode
	:ensure t
	:init
	(add-to-list 'auto-mode-alist '("\\.astro\\'" . astro-ts-mode))
	:hook
	(astro-ts-mode . display-line-numbers-mode))

(use-package jinx
  :hook (text-mode . jinx-mode))

(use-package typst-ts-mode
  :ensure (:type git :host codeberg :repo "meow_king/typst-ts-mode"
                 :files (:defaults "*.el"))
  :custom
	(typst-ts-mode-indent-offset 2)
  (typst-ts-watch-options "--open")
  (typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.dylib" user-emacs-directory))
  (typst-ts-mode-enable-raw-blocks-highlight t))

(use-package diminish)

(setq elpaca-after-init-time (or elpaca-after-init-time (current-time)))
(elpaca-wait)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb
