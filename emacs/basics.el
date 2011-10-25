;; Copyright (C) 1987 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;  Jeff Peck, Sun Microsystems Inc  <peck@sun.com>

(defun ignore-key ()
  "interactive version of ignore"
  (interactive)
  (ignore))

(defun unbound-key ()
  "filler for compound keymaps"
  (interactive)
  (error "unbound-key"))

(defun scroll-down-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))

(defun scroll-up-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-up n))

(defun kill-region-and-unmark (beg end)
  "Like kill-region, but pops the mark [which equals point, anyway.]"
  (interactive "r")
  (kill-region beg end)
  (setq this-command 'kill-region-and-unmark)
  (set-mark-command t))

(defun prev-complex-command ()
  "Select Previous-complex-command"
  (interactive)
  (if (zerop (minibuffer-depth))
      (repeat-complex-command 1)
    (previous-complex-command 1)))

(defun rerun-prev-command ()
  "Repeat Previous-complex-command."
  (interactive)
  (eval (nth 0 command-history)))

(defvar grep-arg nil "Default arg for RE-search")

(defun prev-search-command-arg ()
  ;; if previous minibuf command specified a search string, return it.
  ;; this way, a call to M-x re-search-forward can pass its arg.
  (let* ((command (car command-history))
	 (command-name (symbol-name (car command)))
	 (search-arg (car (cdr command)))
	 (search-command 
	  (and command-name (string-match "search" command-name))))
    (and search-command (stringp search-arg) search-arg)))

(defun grep-arg (&optional prompt)
  "helper function used by research-{backward,forward}"
  (if (memq last-command '(research-forward research-backward)) grep-arg
    (let ((this-command this-command)	; save this binding from read-string
	  (default (or (prev-search-command-arg)
		       search-last-regexp
		       grep-arg)))
      (read-string (or prompt "Regexp arg: ") default))))

(defun research-forward ()
  "Repeat regexp search forward, using previous search arg if available."
  (interactive)				;
  (if (re-search-forward (grep-arg "Regexp search: "))
      (setq search-last-regexp grep-arg)))

(defun research-backward ()
  "Repeat regexp search backward, using previous search arg if available."
  (interactive)				;
  (if (re-search-backward (grep-arg "Regexp search backward: "))
      (setq search-last-regexp grep-arg)))

