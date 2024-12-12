(defun mode-line-window-selected-p ()
  "Return non-nil if we're updating the mode line for the selected window.
This function is meant to be called in `:eval' mode line
constructs to allow altering the look of the mode line depending
on whether the mode line belongs to the currently selected window
or not."
  (let ((window (selected-window)))
    (or (eq window (old-selected-window))
	(and (minibuffer-window-active-p (minibuffer-window))
	     (with-selected-window (minibuffer-window)
	       (eq window (minibuffer-selected-window)))))))

;; Buffer Name
(defun dnsc/modeline--buffer-name ()
  "Return `buffer-name' with spaces around it."
  (format " %s " (buffer-name)))

(defvar-local dnsc/modeline-buffer-name
    '(:eval
      (when (mode-line-window-selected-p)
        (dnsc/modeline--buffer-name)))
  "Mode line construct to display the buffer name.")

(put 'dnsc/modeline-buffer-name 'risky-local-variable t)

(setq mode-line-format
							'("%e"
								dnsc/modeline-buffer-name))
