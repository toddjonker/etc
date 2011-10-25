From cantaloupe.srv.cs.cmu.edu!rochester!rutgers!usc!sdd.hp.com!mips!mips!munnari.oz.au!manuel!cs.anu.edu.au!csis!caroli!peterf Thu Jul  2 19:58:02 1992
Article: 986 of gnu.emacs.sources

Hello!

    I have had lots of help from various people with my little GNU
problems, so I thought I would post some little functions I have
written for Gnu EMACS.  None of them do anything tricky (except for
re-replace-region and vi-type-paren-match, which I didn't write), but
they are all useful when bound to a function key on the keyboard.

    Rather than treat this as an elisp file, it is possibly useful to
pick out a few functions you need and put them in your .emacs, or your
terminal initialization file.

    Many of them simply execute a normal emacs command interactively
with arguments set to a useful value; for example, scroll-down-half is
the same as scroll-down, except that it only scrolls down half a
screen.

    There are a few functions that are appropriate for epoch,
but they are hidden by an (if epoch::version ...).

-peter.
--
;
;  ditties v.0.001
;
;  This file containes lots of useful little functions that I have bound to various keys.
;
;  They're only here because I use them a fair bit and they're not directly provide by emacs.
;
;  I wrote all the functions except for re-replace-region and vi-type-paren-match, which
;  were posted to gnu.emacs.sources a while back (unfortunately, I can't find out who, 
;  so thank you whoever you are!)
; 
; -peter fletcher (peterf@csis.dit.csiro.au)
;
; again                       - Execute last complex command immediately
; beginning-of-window         - Move cursor to top line of window
; byte-compile-buffer         - Save current buffer and byte-compile-file it
; copy-line                   - Copy remainder of line + linefeed to kill buffer.
;                               Use multiple times to copy several lines
; copy-rectangle-as-kill      - Copy rectangle with corners at point and mark; save as last killed one
; cr-no-insert                - Move to beginning of next line
; end-of-next-line            - Move to end of next line
; end-of-prev-line            - Move to end of previous line
; end-of-window               - Move cursor to borrom line of window
; exit-screen (epoch)         - Save buffer, kill buffer and exit screen.
;                               If screen is gnuserv screen, clean it up
; get-removed                 - Inserts stuff deleted by remove-to-eol
; insert-buffer-name          - Insert name of current buffer
; insert-cut-buffer (epoch)   - Insert X cut buffer into current buffer
; insert-date                 - Insert current date into buffer
; insert-file-name            - Insert file name of current buffer
; insert-num-and-increment    - Insert the value of my-num and increment it.
;                               Argument not equal to one sets my-num
; insert-space                - Insert (arg) spaces with no forward movement.
;                               Also inserts in overwrite mode
; kill-whole-line             - Kill whole of line to kill buffer.
;                               Use multiple times to get several lines into kill buffer
; merge-lines                 - Join the next line to the end of the current line.
;                               Leave only 1 space.
; msg-line-column             - Print line and column numbers
; next-screen-now (epoch)     - Move cursor to next screen and pop to top
; re-replace-region           - Replace occurrences of REGEXP with TO-STRING in region.
; remove-to-eol               - Removes up to EOL - use get-removed to reclaim.
;                               Does not disturb kill buffer
; scroll-down-half            - Scroll down half a windowful
; scroll-down-one             - Scroll down one line
; scroll-left-one             - Scroll left one line
; scroll-right-one            - Scroll right one line
; scroll-up-half              - Scroll up half a windowful
; scroll-up-one               - Scroll up one line
; set-selective-display-current-column
;                             - set-selective-display to lines indented less than current-column
; switch-to-buffer-quick      - Switch buffers with no questions asked
; toggle-line-beginning-end   - Toggle between start and end of line
; toggle-match-beginning-end  - Toggle between start and end of last match
; vi-type-paren-match         - Go to the matching parenthesis if on parenthesis.

(provide 'ditties)

;;; RE-REPLACE-REGION replaces OLD (a regular expression) with NEW
;;; throughout the region indicated by BEGIN and END.
;;; For example, to insert a prefix ">" at the beginning of each line
;;; in the region:
;;;   M-x re-replace-regionRET^RET>RET
;;; I don't know who wrote this function!
(defun re-replace-region (begin end old new)
"Replace occurrences of REGEXP with TO-STRING in region."
  (interactive "*r\nsReplace string: \nswith: ")
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (goto-char (point-min))
      (while (re-search-forward old (point-max) t)
        (replace-match new nil nil)))))

(defun vi-type-paren-match (arg)
  "Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond ((looking-at "[([{]") (forward-sexp 1) (backward-char))
	((looking-at "[])}]") (forward-char) (backward-sexp 1))))

(defun toggle-line-beginning-end ()
    "Toggle between start and end of line"
    (interactive)
    (if (zerop (current-column))
      (end-of-line)
      (beginning-of-line)
    )
)

(defun toggle-match-beginning-end ()
    "Toggle between start and end of last match"
    (interactive)
    (let ((match-beginning (match-beginning 0)) (match-end (match-end 0)) (point (point)))
      (if (eq match-end point)
          (goto-char match-begin)
        (goto-char match-end)
        )
      )
)

(setq my-num 0)
(defun insert-num-and-increment (arg)
  "Insert the value of my-num and increment it.  Argument not equal to one sets my-num"
  (interactive "p")
  (if (= arg 1)
      (progn
        (setq my-num (1+ my-num))
        (insert (int-to-string my-num))
      )
      (progn
        (setq my-num arg)
        (insert (int-to-string my-num))
      )
  )
)

(defun insert-date ()
  "Insert current date into buffer"
  (interactive)
  (progn
    (set-mark-command nil)
    (shell-command "date" 1)
    (exchange-point-and-mark)
    (backward-char 1)
    (delete-char 1)
  )
)

(defun byte-compile-buffer ()
  "Save current buffer and byte-compile-file it"
  (interactive)
  (save-buffer)
  (byte-compile-file (buffer-file-name))
)

(defun insert-buffer-name ()
    "Insert name of current buffer"
    (interactive)
    (insert (buffer-name))
)

(defun insert-file-name ()
    "Insert file name of current buffer"
    (interactive)
    (insert (file-name-nondirectory (buffer-file-name)))
)

(defun beginning-of-window ()
  "Move cursor to top line of window"
  (interactive)
  (move-to-window-line 0)
)

(defun end-of-window ()
  "Move cursor to borrom line of window"
  (interactive)
  (move-to-window-line -1)
)

(defun scroll-up-half ()
  "Scroll up half a windowfull"
  (interactive)
  (scroll-up (/ (window-height) 2))
)

(defun scroll-down-half ()
  "Scroll down half a windowfull"
  (interactive)
  (scroll-down (/ (window-height) 2))
)

(defun switch-to-buffer-quick ()
  "Switch buffers with no questions asked"
  (interactive)
  (switch-to-buffer nil t)
)

(defun merge-lines ()
  "Join the next line to the end of the current line.  Leave only 1 space."
  (interactive)
  (end-of-line 1)
  (delete-char 1)
  (just-one-space)
)

(defun kill-whole-line ()
  "Kill whole of line to kill buffer.  Use multiple times to get several lines into kill buffer"
  (interactive)
  (progn
    (beginning-of-line 1)
    (kill-line 1)
  )
)

(defun copy-line ()
  "Copy remainder of line + linefeed to kill buffer.  Use multiple times to copy several lines"
  (interactive)
  (progn
    (set-mark-command nil)
    (next-line 1)
    (beginning-of-line 1)
    (copy-region-as-kill (region-beginning) (region-end))
  )  
)

(defun copy-rectangle-as-kill (start end)
  "Copy rectangle with corners at point and mark; save as last killed one.
Calling from program, supply two args START and END, buffer positions.
But in programs you might prefer to use delete-extract-rectangle."
  (interactive "r")
  (setq killed-rectangle (extract-rectangle start end)))

(defun remove-to-eol ()
  "Removes up to EOL - use get-removed to reclaim.  Does not disturb kill buffer"
  (interactive)
  (progn
    (set-mark-command nil)
    (end-of-line 1)
    (copy-to-register 52 (region-beginning) (region-end) 1)
  )
)

(defun get-removed ()
  "Inserts stuff deleted by remove-to-eol"
  (interactive)
  (progn
    (insert-register 52)
    (exchange-dot-and-mark)
  )
)

(defun end-of-prev-line ()
  "Move to end of previous line"
  (interactive)
  (previous-line 1)
  (end-of-line 1)
)

(defun end-of-next-line ()
  "Move to end of next line"
  (interactive)
  (next-line 1)
  (end-of-line 1)
)

(defun cr-no-insert ()
  "Move to beginning of next line"
  (interactive)
  (next-line 1)
  (beginning-of-line 1)
)

(defun set-selective-display-current-column ()
    "set-selective-display to lines indented less than current-column"
    (interactive)
    (let ( (col (current-column)) )
      (if (zerop col)
          (set-selective-display nil)
          (set-selective-display col)
      )
    )
)

(defun insert-space (arg)
  "Insert (arg) spaces with no forward movement.  Also inserts in overwrite mode"
  (interactive "p")
  (while (> arg 0)
    (insert " ")
    (backward-char 1)
    (setq arg (1- arg))
  )
)

(defun scroll-down-one ()
  "Scroll down one line"
  (interactive)
  (scroll-down 1)
)

(defun scroll-up-one ()
  "Scroll up one line"
  (interactive)
  (scroll-up 1)
)

(defun scroll-right-one ()
  "Scroll right one line"
  (interactive)
  (scroll-right 1)
)

(defun scroll-left-one ()
  "Scroll left one line"
  (interactive)
  (scroll-left 1)
)

(defun again ()
  "Execute last complex command immediately"
  (interactive)
  (eval (car command-history))
)

(defun msg-line-column ()
    "Print line and column numbers"
    (interactive)
    (let ( (col (current-column)) )
      (save-restriction
        (widen)
        (save-excursion
          (beginning-of-line)
          (message "Line: %d Column: %d"
                   (1+ (count-lines 1 (point)))
                   col
          )
        )
      )
    )
)

(if (and (boundp 'epoch::version) epoch::version)
    (progn
      (defun next-screen-now ()
        "Move cursor to next screen and pop to top"
        (interactive)
        (raise-screen (select-screen (next-screen)))
        )
      
      (defun insert-cut-buffer ()
        "Insert X cut buffer into current buffer"
        (interactive)
        (insert (epoch::get-cut-buffer))
        )
      
      (defun exit-screen ()
        "Save buffer, kill buffer and exit screen.  If screen is gnuserv screen, clean it up"
        (interactive)
        (progn
          (if (buffer-modified-p)
              (save-buffer)
            )
          (if (and (boundp 'server-buffer-clients) server-buffer-clients)
              (server-edit nil)
            (progn
              (kill-buffer nil)
              (delete-screen)
              )
            )
          )
        )
      
      )
)
--
Peter Fletcher,            Internet : peterf@csis.dit.csiro.au
PhD Student                Physical : CSIRO Division of Information Technology,
Phone    : +61-6-2750914              ANU, Acton, Canberra ACT AUSTRALIA
-------------------------------------------------------------------------------


