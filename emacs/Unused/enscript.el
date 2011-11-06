;;  Print Emacs buffer via ENSCRIPT
;;  Modified from lpr.el
;;    Todd Jonker  03Nov92


(defconst enscript-switches (list "-G2rq")
  "*List of strings to pass as extra switch args to enscript when it is invoked.")


(defun enscript-buffer ()
  "Print buffer contents as with Unix command `enscript'.
`enscript-switches' is a list of extra switches (strings) to pass along."
  (interactive)
  (enscript-region-1 (point-min) (point-max) enscript-switches))

(defun enscript-region (start end)
  "Print region contents as with Unix command `enscript'.
`enscript-switches' is a list of extra switches (strings) to pass along."
  (interactive "r")
  (enscript-region-1 start end enscript-switches))


(defun enscript-region-1 (start end switches)
  (let ((name (concat (buffer-name) " Emacs buffer"))
	(width tab-width))
    (save-excursion
      (message "Spooling...")
      (if (/= tab-width 8)
	  (let ((oldbuf (current-buffer)))
	    (set-buffer (get-buffer-create " *spool temp*"))
	    (widen) (erase-buffer)
	    (insert-buffer-substring oldbuf start end)
	    (setq tab-width width)
	    (untabify (point-min) (point-max))
	    (setq start (point-min) end (point-max))))
      (apply 'call-process-region
	     (nconc (list start end "enscript"
			  nil nil nil)
		    (nconc (list "-b" name)
			   switches)))
      (message "Spooling...done"))))
