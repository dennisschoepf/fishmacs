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
  :init
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
  (evil-undo-system 'undo-redo) ;; C-r to redo
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

(use-package general
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
	"f" '(:ignore t :wk "[f]ind")
	"f c" '((lambda () (interactive) (find-file "~/.emacs.d/config.org")) :wk "Edit emacs [c]onfig")
	"f s" '(save-buffer :wk "Saves current buffer")
	"f r" '(consult-recent-file :wk "Find [r]ecent files")
	"f f" '(consult-find :wk "Find [f]iles")
	"f g" '(consult-ripgrep :wk "Find with rip[g]rep")
	"f l" '(consult-line :wk "Find [l]ine")
	"f i" '(consult-imenu :wk "Find [i]menu buffer locations"))

  (start/leader-keys
	"b" '(:ignore t :wk "[b]uffers")
	"b b" '(consult-buffer :wk "Switch [b]uffer")
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
	"w l" '(windmove-right :wk "Move to right window"))

  (start/leader-keys
	"n" '(dired-jump :wk "ope[n] dired at current directory"))

  (start/leader-keys
	"B" '(consult-buffer :wk "Show all [B]uffers"))

  (start/leader-keys
	"o" '(:ignore t :wk "[o]pen")
	;; TODO: Add "o t" keybinding to open scratch terminal
	"o d" '(dnsc/dired-open-to-side :wk "Open [d]ired on the side")
	"o l" '(org-agenda :wk "Open al[l] agenda views")
	"o a" '(org-agenda-list :wk "Open [a]genda")
	"o n" '((lambda () (interactive) (org-agenda nil "n")) :wk "[o]pen [n]ext tasks")
	"o c" '(org-capture :wk "[o]rg-[c]apture a new task"))

  (start/leader-keys
	"g" '(:ignore t :wk "[g]it & more")
	"g l" '(git-link :wk "Navigate to git forge [l]ink")
	"g c c" '(comment-line :wk "[g]o [c]omment [c]urrent line")
	"g g" '(magit-status :wk "Ma[g]it status"))

  (start/leader-keys
	"h" '(:ignore t :wk "[h]elp") ;; To get more help use C-h commands (describe variable, function, etc.)
	"h s" '(describe-symbol :wk "Get help for [s]ymbol")
	"h v" '(describe-variable :wk "Get help for [v]ariable")
	"h f" '(describe-function :wk "Get help for [f]unction")
	"h r r" '((lambda () (interactive) (load-file user-init-file)) :wk "Reload Emacs config"))

  (start/leader-keys
	"p" '(:ignore t :wk "[p]rojects") ;; To get more help use C-h commands (describe variable, function, etc.)
	"p p" '(project-switch-project :wk "Switch to another [p]roject")
	"p f" '(project-find-file :wk "[f]ind a file in the project")
	"p g" '(project-find-regexp :wk "[s]earch within project") ;; Maybe use something else here
	"p s" '(project-shell :wk "Open [s]hell within project")
	"p d" '(project-dired :wk "Open [d]ired in project root")
	"p c" '(project-compile :wk "[c]ompile project")
	"p b" '(project-list-buffers :wk "Show project [b]uffers")
	"p k" '(project-kill-buffers :wk "[d]elete all project buffers")
	"p r" '(project-query-replace-regexp :wk "[r]eplace in current project")
	"p x" '(project-async-shell-command :wk "e[x]ecute shell command"))
  
  (start/leader-keys
	"s" '(:ignore t :wk "[s]earch/[s]pell")
	"s c" '(jinx-correct :wk "[c]orrect spelling")
	"s l" '(jinx-languages :wk "Jinx [l]anguages"))

  (start/leader-keys
	"q" '(:ignore t :wk "[q]uit")
	"q q" '(kill-emacs :wk "[q][q]uit Emacs and Daemon")))

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
  (menu-bar-mode nil)         ;; Disable the menu bar
  (scroll-bar-mode nil)       ;; Disable the scroll bar
  (tool-bar-mode nil)         ;; Disable the tool bar
  (inhibit-startup-screen t)  ;; Disable welcome screen
  (visible-bell t)  ;; Disable welcome screen
  (ring-bell-function 'ignore) ;; Disable sounds

  (delete-selection-mode t)   ;; Select text and delete it by typing.
  (electric-indent-mode nil)  ;; Turn off the weird indenting that Emacs does by default.
  (electric-pair-mode t)      ;; Turns on automatic parens pairing

  (blink-cursor-mode nil)     ;; Don't blink cursor
  (global-auto-revert-mode t) ;; Automatically reload file and show changes if the file has changed

  (dired-kill-when-opening-new-dired-buffer t) ;; Dired don't create new buffer
  (recentf-mode t) ;; Enable recent file mode

  (global-visual-line-mode t)           ;; Enable truncated lines
  (display-line-numbers-type 'relative) ;; Relative line numbers
  (global-display-line-numbers-mode t)  ;; Display line numbers

  (mouse-wheel-progressive-speed nil) ;; Disable progressive speed when scrolling
  (scroll-conservatively 10) ;; Smooth scrolling
  (scroll-margin 8)

  (tab-width 4)

  (make-backup-files t) ;; Stop creating ~ backup files
  (auto-save-default nil) ;; Stop creating # auto save files
  
  ;; Use encrypted authinfo file for auth-sources
  (auth-sources '("~/.authinfo.gpg"))
  
  ;; MacOS specific configuration
  (mac-right-option-modifier "none")
  :hook
  (prog-mode . (lambda () (hs-minor-mode t))) ;; Enable folding hide/show globally
  :config
  ;; Move customization variables to a separate file and load it, avoid filling up init.el with unnecessary variables
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)
  :bind (
		 ([escape] . keyboard-escape-quit) ;; Makes Escape quit prompts (Minibuffer Escape)
		 )
  )

(use-package dired-x
  :ensure nil
  :commands (dired-omit-mode)
  :config
  (setq dired-omit-files
	      (concat dired-omit-files "\\|^\\..+$")))

(use-package project
  :ensure nil
  :custom
  (project-vc-ignores '("target/" "bin/" "out/" "node_modules/"))
  (project-vc-extra-root-markers '(".project" "package.json" "Cargo.toml" "go.mod" "Gemfile")))

(use-package catppuccin-theme
  :config
  (load-theme 'catppuccin t t)
  (setq catppuccin-flavor 'mocha)
  (setq catppuccin-italic-comments t)
  (setq catppuccin-enlarge-headings nil)
  (catppuccin-reload))

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

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-total-line-number t)
  (doom-modeline-battery t))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :init
  (vertico-mode))

(savehist-mode) ;; Enables save history mode

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

;; Corfu setup
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Minimum length of prefix for auto completion.
  (corfu-popupinfo-mode t)       ;; Enable popup information
  (corfu-popupinfo-delay 0.5)    ;; Lower popupinfo delay to 0.5 seconds from 2 seconds
  (corfu-separator ?\s)          ;; Orderless field separator, Use M-SPC to enter separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (completion-ignore-case t)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  (corfu-preview-current nil) ;; Don't insert completion without confirmation
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :after corfu
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; The functions that are added later will be the first in the list

  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ;; Complete word from current buffers
  (add-to-list 'completion-at-point-functions #'cape-dict) ;; Dictionary completion
  (add-to-list 'completion-at-point-functions #'cape-file) ;; Path completion
  (add-to-list 'completion-at-point-functions #'cape-elisp-block) ;; Complete elisp in Org or Markdown mode
  (add-to-list 'completion-at-point-functions #'cape-keyword) ;; Keyword/Snipet completion

  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev) ;; Complete abbreviation
  ;;(add-to-list 'completion-at-point-functions #'cape-history) ;; Complete from Eshell, Comint or minibuffer history
  ;;(add-to-list 'completion-at-point-functions #'cape-line) ;; Complete entire line from current buffer
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol) ;; Complete Elisp symbol
  ;;(add-to-list 'completion-at-point-functions #'cape-tex) ;; Complete Unicode char from TeX command, e.g. \hbar
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml) ;; Complete Unicode char from SGML entity, e.g., &alpha
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345) ;; Complete Unicode char using RFC 1345 mnemonics
  )

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

(use-package jinx
  :hook (emacs-startup . global-jinx-mode))

(use-package org
  :ensure nil
  :custom
  (org-directory "~/orgnzr/")
  ;; Fixing source block indentation
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  (org-edit-src-preserve-indentation nil)
  (org-log-done 'note)
  (org-startup-folded t)
  (org-startup-indented t)
  (org-todo-keywords
   '((sequence "PROJECT(p)" "TODO(t)" "NEXT(n)" "|" "DONE(d)")))
  (org-default-notes-file "~/orgnzr/inbox.org")
  (org-agenda-files '("~/orgnzr"))
  (org-refile-use-outline-path 'file)
  (org-refile-targets
   '((nil :maxlevel . 2)
     (org-agenda-files :maxlevel . 2)))
  (org-capture-templates
   '(("t" "Task" entry (file "~/orgnzr/inbox.org")
      "* TODO %?\n %i\n")
     ("l" "Task  line" entry (file "~/orgnzr/inbox.org")
      "* TODO %?\n Relevant line: [[file://%F::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (line-number-at-pos)))]]\n")))
  (org-agenda-custom-commands
   '(("n" todo "NEXT")
     ("p" todo "PROJECT")))
  :hook
  (org-mode . org-indent-mode) ;; Indent text
  (org-mode . (lambda ()
                (setq-local electric-pair-inhibit-predicate
                            `(lambda (c)
                               (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package org-tempo
  :ensure nil
  :after org)

(use-package diminish)

(setq elpaca-after-init-time (or elpaca-after-init-time (current-time)))
(elpaca-wait)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb
