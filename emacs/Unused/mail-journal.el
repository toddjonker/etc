;;;  Mail journal routines
;;;  Todd Jonker
;;;  Rice University
;;;  12 August 1991

;;
;;  Journal a mail message into one of a set of journal files.
;;  Mail messages are cleaned and separated for easy reading.
;;  Journal files are not in RMAIL format, but are text files.
;;
;;  write-journal allows journalling to a specified file, and may be
;;  used interactively.
;;
;;  For automated journalling, first set up journal-mappings.  This
;;  maps used IDs to journal files.  Auto-journalling through sendmail
;;  writes to journals based on the message's TO: header, while auto-
;;  journalling through RMAIL writes to journals based on the message's
;;  FROM: header.
;;
;;  To set up auto-journalling through sendmail, include the following
;;  hook:
;;
;;    (setq mail-mode-hook

(require 'mail-utils)


(defvar journal-last-file nil)

(defconst journal-divider "\n\n\n-------------------------------------\n"
  "*Line used to separate mail journal entries")

; Modified version of rmail-output [rmailout.el]

(defun write-journal (file-name)
  "Append this message to mail journal file named FILE-NAME."
  (interactive
   (list
    (read-file-name
     (concat "Output message to mail journal file"
	     (if journal-last-file
		 (concat " (default "
			 (file-name-nondirectory journal-last-file)
			 "): " )
	       ": "))			
     (and journal-last-file (file-name-directory journal-last-file))
     journal-last-file)))
  (setq file-name (expand-file-name file-name))
  (setq journal-last-file file-name)
  (let ((rmailbuf (current-buffer))
	(tembuf (get-buffer-create " journal-output"))
	(case-fold-search t))
    (save-excursion
      (set-buffer tembuf)
      (erase-buffer)
      (insert journal-divider)
      (insert-buffer-substring rmailbuf)
      (delete-blank-lines)
      (append-to-file (point-min) (point-max) file-name))
    (kill-buffer tembuf))
  (message (concat "Wrote mail to journal " file-name))
  (if (equal major-mode 'rmail-mode)
      (progn
	(rmail-set-attribute "filed" t)
	(and rmail-delete-after-output (rmail-delete-forward)))))



;;;
;;;   Automatic journalling
;;;

;;  Defining journals

(defvar journal-mappings nil
  "Association list mapping userids to journal files.
Use add-journal-mapping to extend the list.")

(defun add-journal-mapping (uid filename)
  "Add a new mapping for auto-journaling.
When UID is encountered in an auto-journal address, 
the message is appended to the journal file FILENAME."
  (setq journal-mappings (cons (cons uid filename) journal-mappings)))


;;  Mail mode

;;  Suggested usage:
;;    (setq mail-mode-hook
;;          '(lambda () (setq mail-send 'mail-send-and-journal)))

; Modified version of mail-send [sendmail.el]

(defun mail-send-and-journal ()
  "Send the message in the current buffer, then journal it.
If  mail-interactive  is non-nil, wait for success indication
or error messages, and inform user.
Otherwise any failure is reported in a message back to
the user from the mailer."
  (interactive)
  (message "Sending...")
  (funcall send-mail-function)
  (set-buffer-modified-p nil)
  (delete-auto-save-file-if-necessary)
  (message "Sending...done")
  (sendmail-auto-journal))

(defun sendmail-auto-journal ()
  "Determine journal files for this message, and write it.
Files are determined by examining the message's To: header."
  (interactive)
  (journal-with-addresses (mail-fetch-field "to")))


;;  Rmail mode

(defun rmail-auto-journal ()
  "Determine journal file for this message, and write it.
File is determined by examining the message's From: header."
  (interactive)
  (journal-with-addresses (mail-fetch-field "from")))


;;  Core routines

(defun journal-with-addresses (addresses)
  "Write buffer to journal files based on address list ADDRESSES.
User IDs are mapped to journal files by journal-mappings."
  (let ((files (journal-process-addresses addresses)))
    (if (null files)
	(message "No automatic journals for this message.")
      (mapcar 'write-journal files))))

(defun journal-process-addresses (addresses)
  "Return list of filenames based on address list ADDRESSES.
That is, for each FOO matching FOO@BAR, find the journal file (if any)
associated with it by journal-mappings."
  (setq addresses (concat " " (mail-strip-quoted-names addresses) " "))
  (let (files)
    ;; Retain only FOO from FOO@BAR or FOO%GOO@BAR
    (while (string-match 
	    "[, ]\\([^, @%]+\\)\\([, ]\\|\\([@%][^, ]+\\)\\)"
	    addresses)
      (let* ((uid (substring addresses (match-beginning 1) (match-end 1)))
	     (id-and-file (assoc uid journal-mappings))
	     (file (if (null id-and-file) nil (cdr id-and-file))))
	
	(if (not (null file)) 
	    (setq files (cons file files))))
      (setq addresses (substring addresses (match-end 0))))
    files))
