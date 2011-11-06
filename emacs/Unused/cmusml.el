;;;
;;; Simple SML mode to go with COMINT package by Olin Shivers.
;;;

(require 'comint)
(provide 'cmusml)

(defvar sml-prompt-pattern "^\\- ")
(defvar explicit-sml-file-name nil)
(defvar cmusml-mode-map '())
(cond ((not cmusml-mode-map)
       (setq cmusml-mode-map (full-copy-sparse-keymap comint-mode-map))))

(defvar cmusml-mode-hook '()
  "*Hook for customizing cmusml mode")

(defun cmusml-mode ()
  "Major mode for interacting with an inferior sml.
Return after the end of the process' output sends the text from the 
    end of process to the end of the current line.
Return before end of process output copies rest of line to end (skipping
    the prompt) and sends it.

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

;;;
;;; Fixed by TVJ
\\{cmusml-mode-map}
Customisation: Entry to this mode runs the hooks on comint-mode-hook and
cmushell-mode-hook (in that order)."
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp sml-prompt-pattern)
  (setq major-mode 'cmusml-mode)
  (setq mode-name "CMU SML")
  (define-key cmusml-mode-map "\C-c\C-d" 'sml-cd)
  (use-local-map cmusml-mode-map)
  (run-hooks 'cmusml-mode-hook))


(defun cmusml ()
  "Run an inferior sml, with I/O through buffer *cmusml*.
If buffer exists but sml process is not running, make new sml.
If buffer exists and sml process is running, just switch to buffer *cmusml*.
Program used comes from variable explicit-sml-file-name,
 or (if that is nil) from the SML environment variable.
If a file ~/.emacs_sml exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in cmusml-mode, giving commands for sending input
and controlling the subjobs of the sml.  See cmusml-mode.
See also variable sml-prompt-pattern.

\(Type \\[describe-mode] in the sml buffer for a list of commands.)"
  (interactive)
  (cond ((not (comint-check-proc "*cmusml*"))
	 (let* ((prog (or explicit-sml-file-name
			  (getenv "SML")
			  "sml"))
		(name (file-name-nondirectory prog))
		(startfile (concat "~/.emacs_" name)))
	   (set-buffer (apply 'make-comint "cmusml" prog
			      (if (file-exists-p startfile) startfile)))
	   (cmusml-mode))))
  (switch-to-buffer "*cmusml*"))

(defun sml-cd (dir)
  (interactive "DSML Directory: ")
  (cd (expand-file-name dir))
  (let ((sml-process (get-buffer-process "*cmusml*")))
    (process-send-string sml-process
     (concat "System.Directory.cd \"" dir "\";\"" dir "\";\n"))))
