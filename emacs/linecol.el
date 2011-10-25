;;; NAME:	 line-column.el
;;; SYNOPSIS:	 A GNU Emacs extension which displays current line and
;;;		 column number in the mode-line.
;;; VERSION:	 0.3
;;; LAST CHANGE: 950703
;;; CREATED:	 931108
;;; AUTHOR:	 Mikael Djurfeldt <djurfeldt@nada.kth.se>
;;; COPYRIGHT:	 (C) Mikael Djurfeldt 1993, 1995
;;;
;;;  Verbatim copies of this file may be freely redistributed.
;;;
;;;  Modified versions of this file may be redistributed provided that this
;;;  notice remains unchanged, the file contains prominent notice of
;;;  author and time of modifications, and redistribution of the file
;;;  is not further restricted in any way.
;;;
;;;  This file is distributed `as is', without warranties of any kind.
;;;
;;; REQUIREMENTS:
;;;  Emacs-19.29 or later
;;;
;;; USAGE:
;;;  Issue the command `line-column-mode' in the buffer where you want
;;;  line and column numbers displayed.  Restore the mode-line by
;;;  issuing the command again.  It works as a toggle.
;;;
;;; BUGS:
;;;  The customization variables only take effect when
;;;  line-column-mode is first envoked.
;;;

;;;###autoload
(defvar line-column-format "(%4l,%3c)  "
  "*Template for displaying line and column in the mode line.
Use %l for line number and %c for column number.")

;;;###autoload
(defvar line-column-variable 'global-mode-string
  "*Tells line-column-mode where to put the line and column information.
Suggested values are `global-mode-string' or `mode-line-format'.")

;;;###autoload
(defvar line-column-prepend t
  "*Controls where in a mode line variable the line and column info goes.
If non-nil, line and column info is prepended otherwise appended.
See documentation for variable `line-column-variable'.")

;;;###autoload
(defvar line-column-mode nil
  "*Non-nil means display line and column numbers in mode line.")

;;;###autoload
(defun line-column-mode (arg)
  "*Toggle line-column-mode.
With arg, turn line-column-mode on if and only if arg is positive.
When line-column-mode is enabled, the line and column of the point
appears in the mode line."
  (interactive "P")
  (setq line-column-mode
	(if (null arg) (not line-column-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if (not line-column-mode)
      (remove-hook 'post-command-hook 'force-mode-line-update)
    ;; Enter line-column-mode
    (if (not (assq 'line-column-mode
		   (symbol-value line-column-variable)))
	(set line-column-variable
	     (if line-column-prepend
		 (cons (list 'line-column-mode line-column-format)
		       (symbol-value line-column-variable))
	       (append (symbol-value line-column-variable)
		       (list (list 'line-column-mode
				   line-column-format))))))
    (add-hook 'post-command-hook 'force-mode-line-update)))

;;; EOF
