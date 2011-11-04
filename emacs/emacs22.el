;;;   todd's .emacs stuff for Emacs 22			-*-emacs-lisp-*-
;;;


;;;
;;;   Preferences
;;;

(show-paren-mode 1)

;; Keep the mouse pointer away from the cursor.  Cool!
(mouse-avoidance-mode 'animate)


(defun my-scroll-up ()
  (interactive)
  (scroll-up 1))

(defun my-scroll-down ()
  (interactive)
  (scroll-down 1))

;;;
;;;   Key bindings
;;;

;; Make ^Z act like ^X-5
(define-key global-map "\C-z" ctl-x-5-map)

(define-key global-map [M-up] 'up-20)
(define-key global-map [M-down] 'down-20)

(define-key global-map [M-S-up] 'my-scroll-up)
(define-key global-map [M-S-down] 'my-scroll-down)

(define-key global-map [M-delete] 'kill-word)
;(define-key global-map [ctrl delete] 'undefined-key)

(define-key global-map [f1] 'find-file)
(define-key global-map [f2] 'find-file-other-frame)
(define-key global-map [f3] 'revert-buffer)
(define-key global-map [f4] 'bury-buffer)
;(define-key global-map [f5] 'rmail)

(define-key global-map [f6] 'repeat-complex-command)
(define-key global-map [f7] 'query-replace)
(define-key global-map [f8] 'toggle-tab-width)
(define-key global-map [f9] 'toggle-read-only)
(define-key global-map [f10] 'toggle-debugger)

(define-key global-map [f11] 'glue-lines)
(define-key global-map [f12] 'fill-paragraph)
(define-key global-map [f13] 'glue-lines)
(define-key global-map [f14] 'fill-paragraph)


(define-key global-map [f17] 'beginning-of-buffer)
(define-key global-map [f18] 'end-of-buffer)
(define-key global-map [f19] 'goto-line)
(define-key global-map [f20] 'what-line)

(define-key global-map [home]	'beginning-of-line)
(define-key global-map [end]	'end-of-line)
(define-key global-map [find]	'query-replace)
(define-key global-map [insert] 'overwrite-mode)
(define-key global-map [remove] 'undefined-key)



;; Customizations for both c-mode and c++-mode
(defun my-e19-c-mode-hook ()
  ;; Get rid of c-mark-function:
  (define-key c-mode-map [meta backspace] 'backward-kill-word)
  )
(add-hook 'c-mode-common-hook 'my-e19-c-mode-hook 'append)


;; For some reason this didn't work, so I hacked insert-line to check
;; the active major mode.
'(defun my-e19-c++-mode-hook ()
  ;; Keybindings for C++.
  (define-key c++-mode-map [meta control l] 'insert-c++-comment-line)
  '(local-set-key [meta control l] 'insert-c++-comment-line)
  )
'(add-hook 'c++-mode-hook 'my-e19-c++-mode-hook 'append)




;;;==========================================================================
;;;
;;;   Setup for windows-based editing.
;;;

(defun my-win32-setup ()

  ;; A smaller font is "-*-Terminal-normal-r-*-*-8-60-*-*-c-60-ms-oem")
  ;(set-default-font "-*-Courier New-normal-r-*-*-12-90-*-*-c-*-*-iso8859-l")


  '(setq default-frame-alist 
	'((font . "-*-Terminal-normal-r-*-*-8-60-*-*-c-60-ms-oem")
	  (width  . 80)
	  (height . 80)))
  ;; Small font can't display bold, so change color instead.
  '(set-face-foreground 'bold      "blue")


  '(setq default-frame-alist 
	'((font . "-*-Courier New-normal-r-*-*-12-90-*-*-c-*-*-iso8859-l")
	  (width  . 80)
	  (height . 50)))


  (require 'info)
  (set-face-foreground 'info-node "blue")
  (set-face-foreground 'info-xref "blue")


  ;; Start the Windows server process.
  (if (load "gnuserv" 'noerror 'nomessage)
      (gnuserv-start))

  (setq w32-quote-process-args ?\")


  ;; Configure things for Cygnus tools
  (let ((cygwin-bin "c:/cygnus/cygwin-b20/H-i586-cygwin32/bin/"))

    '(setq Info-directory-list (cons "C:/cygnus/cygwin-b20/info/"
				    Info-directory-list))

    (setenv "PATH" (concat (concat cygwin-bin ";")
			   (getenv "PATH")))
    (setq exec-path (cons cygwin-bin exec-path))

    ;; For subprocesses invoked via the shell (e.g., "shell -c command")
    (setq shell-file-name (concat cygwin-bin "bash.exe"))
    ;; For the interactive shell
    (setq explicit-shell-file-name shell-file-name)
    (setenv "SHELL" shell-file-name)

    (defun my-shell-setup ()
      "For bash (cygwin 18) under Emacs 20"
      (setq comint-scroll-show-maximum-output t)
      (setq comint-completion-addsuffix t)
      (setq comint-eol-on-send t)
      '(make-variable-buffer-local 'comint-completion-addsuffix))
    (setq shell-mode-hook 'my-shell-setup)

    (setq process-coding-system-alist (cons '("bash" . raw-text-unix)
					    process-coding-system-alist))
    )

  )


;;;---------------------------------------------------------------------------


(defun my-xwindow-setup ()

  ; Force the super key to act as meta.
  ; The Kinesis Ctr/Cmd key looks like Super to X
  ; Simply remapping that worked in X but Synergy couldn't map it back to Cmd
  ; This is annoying, I'd rather handle this all in X and Synergy so its the
  ; same throughout X applications.
  (setq x-super-keysym 'meta)

  (setq default-frame-alist '(;(minibuffer . nil)
			      (font . "6x13")
			      (width  . 80)
			      (height . 50)
			      (paneFont . "9x15")
			      (selectionFont . "9x15")))

  '(setq minibuffer-frame-alist '((font . "8x13")
				 (width  . 96)
				 (height . 1)
				 (autoraise . t)))

  (setq initial-frame-alist '(;(minibuffer . nil)
			      (width  . 80)
			      (height . 50)))
  )


;;;---------------------------------------------------------------------------

(if (not (eq window-system nil))
    (progn
      ;; Common code for all window systems

      ;; Remove the binding of C-x C-c, which normally exits emacs.
      ;; It's easy to hit this by mistake, and that can be annoying.
      ;; Under X, you can always quit with the "Exit Emacs" option on
      ;; the Files menu.
      '(global-set-key "\C-x\C-c" nil)
      ;; Disabled 2009-07-10 because I find I want this more often now.

      ;; Turn on font-lock mode all over the place!  I like it!
      (require 'font-lock)
      (setq font-lock-global-modes t)
      (global-font-lock-mode t)
      (set-face-foreground 'font-lock-function-name-face "blue")
      (set-face-foreground 'font-lock-comment-face "red")
      (set-face-foreground 'font-lock-string-face "forest green")

      (cond 
       ((eq window-system 'x)   (my-xwindow-setup))
       ((eq window-system 'w32) (my-win32-setup))
       ))
  )



;; EOF ;;
