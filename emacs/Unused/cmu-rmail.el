;;; CMU modifications to RMAIL to make it work with local eccentricities.
;;;
;;; HISTORY
;;;
;;; 07-Jan-87 - Created by Stewart Clamen
;;;
;;; 12-Feb-87 - Added hooks for "cmu-rmailsum".
;;;
;;; 09-Sep-87 - Created "cmu-rmail" feature, so that other libraries
;;;             can use it conveniently.
;;;
;;; 14-May-89 - Converted to GNU Emacs 18.54
;;;
;;; 23-Oct-91 - Modified to run under Epoch
;;;

(provide 'cmu-rmail)
(require 'rmail)

; (defconst movemail-program
;   (expand-file-name "cmu-movemail" exec-directory)
;   "*Name of movemail program")


(defconst movemail-program "/usr/misc/.gnu-emacs/etc/cmu-movemail"
  "*Name of movemail program")


;;old one
(defun rmail-insert-inbox-text (files renamep)
  (let (file tofile delete-files movemail)
    (while files
      (setq file (substitute-in-file-name (car files))
	    tofile (concat file "~"))
      ;; If getting from mail spool directory,
      ;; use movemail to move rather than renaming.
      (setq movemail (equal (file-name-directory file) rmail-spool-directory))
      (if movemail
	  (progn
	    (setq tofile (expand-file-name "~/.newmail"))
	    ;; On some systems, /usr/spool/mail/foo is a directory
	    ;; and the actual inbox is /usr/spool/mail/foo/foo.
	    (if (file-directory-p file)
		(setq file (substitute-in-file-name
			     (concat file "/$USER"))))))
      (message "Looking for new mail")
      ;; If not renaming, get straight out of the actual inbox.
      ;; If renaming, get out of the alternate name if that exists,
      ;; otherwise rename the inbox to the alternate name and get from it.
      (if renamep
	  (if (and (not (file-exists-p tofile)) (file-exists-p file))
	      (if movemail
		  (call-process movemail-program nil nil nil file tofile)
		(rename-file file tofile nil)))
	(setq tofile file))
      ;; At this point, tofile contains the name to read.
      ;; Either the alternate name (if we renamed)
      ;; or the actual inbox (if not renaming).
      (if (file-exists-p tofile)
	  (progn (goto-char (point-max))
		 (insert-file-contents tofile)
		 (goto-char (point-max))
		 (or (= (preceding-char) ?\n)
		     (insert ?\n))
		 (setq delete-files (cons tofile delete-files))))
      (setq files (cdr files)))
    delete-files))

;;new one
(defun rmail-insert-inbox-text (files renamep)
  (let (file tofile delete-files movemail)
    (while files
      (setq file (expand-file-name (substitute-in-file-name (car files)))
	    ;;>> un*x specific <<
	    tofile (concat file "~"))
      ;; If getting from mail spool directory,
      ;; use movemail to move rather than renaming.
      (setq movemail (equal (file-name-directory file) rmail-spool-directory))
      (if movemail
	  (progn
	    (setq tofile (expand-file-name
			   ".newmail"
			   (file-name-directory
			     (expand-file-name rmail-file-name))))
	    ;; On some systems, /usr/spool/mail/foo is a directory
	    ;; and the actual inbox is /usr/spool/mail/foo/foo.
	    (if (file-directory-p file)
		(setq file (substitute-in-file-name
			     (expand-file-name "$USER" file))))))
      (if (or (file-exists-p tofile) (file-exists-p file))
	  (message "Getting mail from %s..." file))
      ;; Set TOFILE if have not already done so, and
      ;; rename or copy the file FILE to TOFILE if and as appropriate.
      (cond ((not renamep)
	     (setq tofile file))
	    ((or (file-exists-p tofile) (not (file-exists-p file)))
	     nil)
	    ((not movemail)
	     (rename-file file tofile nil))
	    (t
	     (let ((errors nil))
	       (unwind-protect
		   (save-excursion
		     (setq errors (generate-new-buffer " *rmail loss*"))
		     (buffer-flush-undo errors)
		     (call-process movemail-program
				   nil errors nil file tofile)
		     (if (not (buffer-modified-p errors))
			 ;; No output => movemail won
			 nil
		       (set-buffer errors)
		       (subst-char-in-region (point-min) (point-max)
					     ?\n ?\  )
		       (goto-char (point-max))
		       (skip-chars-backward " \t")
		       (delete-region (point) (point-max))
		       (goto-char (point-min))
		       (if (looking-at "movemail: ")
			   (delete-region (point-min) (match-end 0)))
		       (signal 'file-error
			       (list "movemail"
				     (buffer-substring (point-min)
						       (point-max))
				     ;file tofile
				     ))))
		 (if errors (kill-buffer errors))))))
      ;; At this point, TOFILE contains the name to read:
      ;; Either the alternate name (if we renamed)
      ;; or the actual inbox (if not renaming).
      (if (file-exists-p tofile)
	  (progn (goto-char (point-max))
		 (insert-file-contents tofile)
		 (goto-char (point-max))
		 (or (= (preceding-char) ?\n)
		     (insert ?\n))
		 (setq delete-files (cons tofile delete-files))))
      (message "")
      (setq files (cdr files)))
    delete-files))

;;old one
;; the  rmail-break-forwarded-messages  feature is not implemented
(defun rmail-convert-to-babyl-format ()
  (let ((count 0) start
	(case-fold-search t))
    (goto-char (point-min))
    (save-restriction
      (while (not (eobp))
	(cond ((looking-at "Babyl Options:");Babyl header
	       (search-forward "\n\^_")
	       (delete-region (point-min) (point)))
	      ;; Babyl format message
	      ((looking-at "\^L")
	       (or (search-forward "\n\^_" nil t)
		   (progn
		     (message "Invalid Babyl format in inbox!")
		     (sit-for 1)
		     (goto-char (point-max))))
	       (setq count (1+ count))
	       (skip-chars-forward " \t\n")
	       (narrow-to-region (point) (point-max)))
	      ;;*** MMDF format
	      ((looking-at mmdf-delim1)
	       (replace-match "\^L\n0,unseen,,\n*** EOOH ***\n")
	       (setq start (point))
	       (re-search-forward mmdf-delim2 nil t)
	       (replace-match "\^_")
	       (save-excursion
		 (save-restriction
		   (narrow-to-region start (1- (point)))
		   (goto-char (point-min))
		   (while (search-forward "\n\^_" nil t); single char "\^_"
		     (replace-match "\n^_")))); 2 chars: "^" and "_"
	       (narrow-to-region (point) (point-max))
	       (setq count (1+ count)))
	      ;;*** Mail format (CMU Mail)
 	      ((looking-at "^\\(From\\|Received:\\|Date:\\) ")
	       (setq start (point))
	       (insert "\^L\n0,unseen,,\n*** EOOH ***\n")
	       (rmail-nuke-pinhead-header)
	       (if (re-search-forward
		    (concat "^[\^_]?\\("
 			    "\\(From\\|Received:\\|Date:\\)"
 			    " [^ \n]*\\(\\|\".*\"[^ \n]*\\)  ?[^ \n]* [^ \n]* *"
 			    "[0-9]* [0-9:]* \\(..T \\|\\)" ; EDT
			    "19[0-9]*$\\|"
			    mmdf-delim1 "\\|"
			    "^Babyl Options:\\|"
			    "\^L\n[01],\\)") nil t 2)
		   (goto-char (match-beginning 1))
		 (goto-char (point-max)))
	       (setq count (1+ count))
	       (save-excursion
		 (save-restriction
		   (narrow-to-region start (point))
		   (goto-char (point-min))
		   (while (search-forward "\n\^_" nil t); single char
		     (replace-match "\n^_")))); 2 chars: "^" and "_"
	       (insert ?\^_)
	       (narrow-to-region (point) (point-max)))
	      ;; ***CMU format (system mailbox)
	      ((looking-at "^\^C\n")
	       (kill-line 1)		;remove delimiter
	       (setq start (point))
	       (insert "\^L\n0,unseen,,\n*** EOOH ***\n")
	       (rmail-nuke-pinhead-header)
	       (cond ((re-search-forward
		       (concat "^[\^_]?\\("
			       mmdf-delim1 "\\|"
			       "^\^C\n\\|"
			       "^Babyl Options:\\|"
			       "\^L\n[01],\\)")
			       nil t)
		      (goto-char (match-beginning 1)))
		     ((re-search-forward
		       (concat "^[\^_]?\\("
			       "\\(From\\|Received:\\|Date:\\)"
			       " [^ \n]*\\(\\|\".*\"[^ \n]*\\)  ?[^ \n]* [^ \n]* *"
			       "[0-9]* [0-9:]* \\(..T \\|\\)" ; EDT
			       "19[0-9]*$\\)") nil t 2)
		      (goto-char (match-beginning 1)))
		     (t (goto-char (point-max))))
	       (setq count (1+ count))
	       (save-excursion
		 (save-restriction
		   (narrow-to-region start (point))
		   (goto-char (point-min))
		   (while (search-forward "\n\^_" nil t); single char
		     (replace-match "\n^_")))); 2 chars: "^" and "_"
	       (insert ?\^_)
	       (narrow-to-region (point) (point-max)))
	      ;;
	      ;;This is a kludge, in case we're wrong about mmdf not
	      ;;allowing anything in between.  If it loses, we'll have
	      ;;to look for something else
	      (t (delete-char 1)))))
    count))

;;new one
(defun rmail-convert-to-babyl-format ()
  (let ((count 0) start
	(case-fold-search nil))
    (goto-char (point-min))
    (save-restriction
      (while (not (eobp))
	(cond ((looking-at "Babyl Options:");Babyl header
	       (search-forward "\n\^_")
	       (delete-region (point-min) (point)))
	      ;; Babyl format message
	      ((looking-at "\^L")
	       (or (search-forward "\n\^_" nil t)
		   (progn
		     (message "Invalid Babyl format in inbox!")
		     (sit-for 1)
		     (goto-char (point-max))))
	       (setq count (1+ count))
	       (skip-chars-forward " \t\n")
	       (narrow-to-region (point) (point-max)))
	      ;;*** MMDF format
	      ((let ((case-fold-search t))
		 (looking-at mmdf-delim1))
	       (let ((case-fold-search t))
		 (replace-match "\^L\n0, unseen,,\n*** EOOH ***\n")
		 (setq start (point))
		 (re-search-forward mmdf-delim2 nil t)
		 (replace-match "\^_"))
	       (save-excursion
		 (save-restriction
		   (narrow-to-region start (1- (point)))
		   (goto-char (point-min))
		   (while (search-forward "\n\^_" nil t) ; single char "\^_"
		     (replace-match "\n^_")))) ; 2 chars: "^" and "_"
	       (narrow-to-region (point) (point-max))
	       (setq count (1+ count)))
	      ;;*** Mail format (CMU Mail)
 	      ((looking-at "^\\(From\\|Received:\\|Date:\\) ")
	       (setq start (point))
	       (insert "\^L\n0, unseen,,\n*** EOOH ***\n")
	       (rmail-nuke-pinhead-header)
	       (if (re-search-forward
		    (concat "^[\^_]?\\("
 			    "\\(From\\|Received:\\|Date:\\)"
 			    " [^ \n]*\\(\\|\".*\"[^ \n]*\\)  ?[^ \n]* [^ \n]* *"
 			    "[0-9]* [0-9:]* \\(..T \\|\\)" ; EDT
			    "19[0-9]*$\\|"
			    mmdf-delim1 "\\|"
			    "^Babyl Options:\\|"
			    "\^L\n[01],\\)") nil t 2)
		   (goto-char (match-beginning 1))
		 (goto-char (point-max)))
	       (setq count (1+ count))
	       (save-excursion
		 (save-restriction
		   (narrow-to-region start (point))
		   (goto-char (point-min))
		   (while (search-forward "\n\^_" nil t); single char
		     (replace-match "\n^_")))); 2 chars: "^" and "_"
	       (insert ?\^_)
	       (narrow-to-region (point) (point-max)))
	      ;; ***CMU format (system mailbox)
	      ((looking-at "^\^C\n")
	       (kill-line 1)		;remove delimiter
	       (setq start (point))
	       (insert "\^L\n0, unseen,,\n*** EOOH ***\n")
	       (rmail-nuke-pinhead-header)
	       (cond ((re-search-forward
		       (concat "^[\^_]?\\("
			       mmdf-delim1 "\\|"
			       "^\^C\n\\|"
			       "^Babyl Options:\\|"
			       "\^L\n[01],\\)")
			       nil t)
		      (goto-char (match-beginning 1)))
		     ((re-search-forward
		       (concat "^[\^_]?\\("
			       "\\(From\\|Received:\\|Date:\\)"
			       " [^ \n]*\\(\\|\".*\"[^ \n]*\\)  ?[^ \n]* [^ \n]* *"
			       "[0-9]* [0-9:]* \\(..T \\|\\)" ; EDT
			       "19[0-9]*$\\)") nil t 2)
		      (goto-char (match-beginning 1)))
		     (t (goto-char (point-max))))
	       (setq count (1+ count))
	       (save-excursion
		 (save-restriction
		   (narrow-to-region start (point))
		   (goto-char (point-min))
		   (while (search-forward "\n\^_" nil t); single char
		     (replace-match "\n^_")))); 2 chars: "^" and "_"
	       (insert ?\^_)
	       (narrow-to-region (point) (point-max)))
	      ;;
	      ;;This is a kludge, in case we're wrong about mmdf not
	      ;;allowing anything in between.  If it loses, we'll have
	      ;;to look for something else
	      (t (delete-char 1)))))
    count))

;;; *** Refill message ***

(defun rmail-refill-message ()
  "Refill current message.  This fixes messages from people who send
messages with lines wrapped around"
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (fill-individual-paragraphs (point) (point-max) nil t))))


(define-key rmail-mode-map "\\" 'rmail-refill-message)


;;; Adding documentation
(defun rmail-mode ()
  "Rmail Mode is used by \\[rmail] for editing Rmail files.
All normal editing commands are turned off.
Instead, these commands are available:

.	Move point to front of this message (same as \\[beginning-of-buffer]).
SPC	Scroll to next screen of this message.
DEL	Scroll to previous screen of this message.
n	Move to Next non-deleted message.
p	Move to Previous non-deleted message.
M-n	Move to Next message whether deleted or not.
M-p	Move to Previous message whether deleted or not.
>	Move to the last message in Rmail file.
j	Jump to message specified by numeric position in file.
M-s	Search for string and show message it is found in.
d	Delete this message, move to next nondeleted.
C-d	Delete this message, move to previous nondeleted.
u	Undelete message.  Tries current message, then earlier messages
	till a deleted message is found.
e	Expunge deleted messages.
s	Expunge and save the file.
q       Quit Rmail: expunge, save, then switch to another buffer.
C-x C-s Save without expunging.
g	Move new mail from system spool directory or mbox into this file.
m	Mail a message (same as \\[mail-other-window]).
c	Continue composing outgoing message started before.
r	Reply to this message.  Like m but initializes some fields.
f	Forward this message to another user.
o       Output this message to an Rmail file (append it).
C-o	Output this message to a Unix-format mail file (append it).
i	Input Rmail file.  Run Rmail on that file.
a	Add label to message.  It will be displayed in the mode line.
k	Kill label.  Remove a label from current message.
C-M-n   Move to Next message with specified label
          (label defaults to last one specified).
          Standard labels: filed, unseen, answered, forwarded, deleted.
          Any other label is present only if you add it with `a'.
C-M-p   Move to Previous message with specified label
C-M-h	Show headers buffer, with a one line summary of each message.
C-M-l	Like h only just messages with particular label(s) are summarized.
C-M-r   Like h only just messages with particular recipient(s) are summarized.
t	Toggle header, show Rmail header if unformatted or vice versa.
w	Edit the current message.  C-c C-c to return to Rmail.
\"	Remove AMS garbage from message.
\\       Refill current message. 
"
  (interactive)
  (kill-all-local-variables)
  (rmail-mode-1)
  (rmail-variables)
  (run-hooks 'rmail-mode-hook))


;;; AMS Unsupport 

(defconst ams-identifying-mail-field
  "X-Andrew-Message-Size"
  "Mail field identifying message as using AMS features")

(defun rmail-remove-ams-garbage ()
  "Remove AMS stuff from current message."
  (interactive)
  (if (mail-fetch-field ams-identifying-mail-field)
      (save-excursion
	(let ((buffer-read-only nil))
	  (convert-from-ams-format-in-buffer)
	  (goto-char (point-min))
	  (re-search-forward ams-identifying-mail-field)
	  (beginning-of-line)
	  (insert "Comments:  Originally included: ")
	  ))
    (message "Message is not in AMS format")))

;;; Binding to match help mode
(define-key rmail-mode-map "\"" 'rmail-remove-ams-garbage)

(autoload 'convert-from-ams-format-in-buffer "amscleanup"
  "Remove AMS references from buffer."
  t)


;;;; *** Show-message-hook ***

(defun rmail-comment-on-ams-message ()
  "Default hook for show-message-hook in cmu-rmail module.\n
Check to see if message is in AMS format and hint that there is a command
that attempts to fix it.
"
  (if (mail-fetch-field "X-Andrew-Message-Size")
      (setq blurb "This message is in AMS format. Type \" to repair.")))

(setq rmail-show-message-hook 'rmail-comment-on-ams-message)


;;;; *** Mail summaries *** (redirected)


(autoload 'rmail-summary "cmu-rmailsum"
  "Display a summary of all messages, one line per message.

Includes minor modifications to have summary lines handle CMU
\"First.Last\" FROM fields more nicely."
   t)

(autoload 'rmail-summary-by-labels "cmu-rmailsum"
  "Display a summary of all messages with one or more LABELS.
LABELS should be a string containing the desired labels, separated by commas.

Includes minor modifications to have summary lines handle CMU
\"First.Last\" FROM fields more nicely."
  t)

(autoload 'rmail-summary-by-recipients "cmu-rmailsum"
  "Display a summary of all messages with the given RECIPIENTS.
Normally checks the To, From and Cc fields of headers;
but if PRIMARY-ONLY is non-nil (prefix arg given),
 only look in the To and From fields.
RECIPIENTS is a string of names separated by commas.

Includes minor modifications to have summary lines handle CMU
\"First.Last\" FROM fields more nicely."
  t)

