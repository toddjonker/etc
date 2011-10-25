
(require 'itimer)

(defvar ebiff::icon-mailempty (make-pixmap "flagdown"))
(defvar ebiff::icon-mailfull  (make-pixmap "flagup"))

;(defvar ebiff::icon-mailempty (make-pixmap "letters"))
;(defvar ebiff::icon-mailfull  (make-pixmap "noletters"))

;(defvar ebiff::icon-mailempty (make-pixmap "mailempty"))
;(defvar ebiff::icon-mailfull  (make-pixmap "mailfull"))

(defvar ebiff::mail-file nil
  "*File name of mail inbox file, for indicating existence of new mail.
Default is system-dependent, and is the same as used by Rmail.")

(defvar ebiff::interval 60
  "*Seconds between updates of mail message in the mode line.")

(defvar ebiff::use-echo-area nil
  "*If non-nil, ebiff will use the echo area instead of the mode line.")

(defvar ebiff::hooks '(ebiff::update-mailbox-icon))


;;;
;;; Private vars
;;;

(defvar ebiff::message-string nil)

(defvar ebiff::mailfull nil
  "If non-nil, mailbox was full last time we checked.")

(defun ebiff::start ()
  (interactive)
  ;; if the "ebiff" itimer already exists, nuke it first.
  (let ((old (get-itimer "ebiff")))
    (if old (delete-itimer old)))
  ;; If we're not displaying the message in the echo area
  ;; and the global mode string does not have a non-nil value
  ;; then initialize the global mode string's value.
  (or ebiff::use-echo-area
      global-mode-string
      (setq global-mode-string '("")))
  ;; If we're not displaying the message in the echo area
  ;; and our display variable is not part of the global-mode-string list
  ;; the we add our variable to the list.  This will make the message
  ;; appear on the modeline.
  (or ebiff::use-echo-area
      (memq 'ebiff::message-string global-mode-string)
      (setq global-mode-string
	    (append global-mode-string '(ebiff::message-string))))
  ;; Display the initial message...
  (setq ebiff::mailfull 'unknown)
  (ebiff::timer-function)
  ;; ... and start an itimer to do it automatically thereafter.
  ;;
  ;; If we wanted to be really clever about this, we could have the itimer
  ;; not be automatically restarted, but have it re-add itself each time.
  ;; Then we could look at (current-time) and arrange for the itimer to
  ;; wake up exactly at the minute boundary.  But that's just a little
  ;; more work than it's worth...
  (start-itimer "ebiff" 'ebiff::timer-function
		ebiff::interval ebiff::interval))


(defun ebiff::timer-function ()
  (let* ((load (format " %03d" (let ((debug-on-error nil) ;fmh
				     (stack-trace-on-error nil))
				 (condition-case ()
				     (car (load-average))
				   (error 0)))))
	 (mail-spool-file (or ebiff::mail-file
			      (getenv "MAIL")
			      (concat rmail-spool-directory
				      (or (getenv "LOGNAME")
					  (getenv "USER")
					  (user-login-name)))))
	 (got-mail (and (file-exists-p mail-spool-file)
			;; file not empty?
			(< 0 (nth 7 (file-attributes
				     (file-chase-links mail-spool-file))))))
	 (string (if got-mail "Mail" "")))

    ;; Run hooks when mail status changes
    (let ((old-status ebiff::mailfull))
      (setq ebiff::mailfull got-mail)
      (if (not (eq old-status got-mail))
	  (run-hooks 'ebiff::hooks)))

    (if ebiff::use-echo-area
	(if got-mail
	    (or (> (minibuffer-depth) 0)
		;; don't stomp echo-area-buffer if reading from minibuffer now.
		(save-excursion
		  (save-window-excursion
		    (let ((string "You have new mail"))
		      (select-window (minibuffer-window))
		      (erase-buffer)
		      (indent-to (- (screen-width) (length string) 1))
		      (insert "You have new mail")
		      (message (buffer-string)))))))
      (setq ebiff::message-string string)
      ;; Force redisplay of all buffers' mode lines to be considered.
      (save-excursion (set-buffer (other-buffer)))
      (set-buffer-modified-p (buffer-modified-p))
      ;; Do redisplay right now, if no input pending.
      (sit-for 0))))

(defvar ebiff::icon-screen nil)
(defun ebiff::update-mailbox-icon ()
  (if ebiff::icon-screen
      (save-excursion
	(save-window-excursion
	  (x-set-screen-icon-pixmap ebiff::icon-screen
				    (if ebiff::mailfull ebiff::icon-mailfull
				      ebiff::icon-mailempty))))))

(provide 'ebiff)

;; EOF ;;
