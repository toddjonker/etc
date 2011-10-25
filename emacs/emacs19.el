;;;   todd's .emacs stuff for Emacs 19			-*-emacs-lisp-*-
;;;


;;;
;;;   New preferences
;;;

;(setq pop-up-frames t)



;;;
;;;   Key bindings
;;;

;; Make ^Z act like ^X-5
(define-key global-map "\C-z" ctl-x-5-map)

(define-key global-map [M-up] 'up-20)
(define-key global-map [M-down] 'down-20)


; These don't seem to work!
(define-key global-map [M-delete] 'kill-word)
(define-key global-map [ctrl delete] 'undefined-key)

(define-key global-map [f1] 'find-file)
(define-key global-map [f2] 'find-file-other-frame)
(define-key global-map [f3] 'revert-buffer)
(define-key global-map [f4] 'bury-buffer)
;(define-key global-map [f5] 'rmail)

(define-key global-map [f6] 'repeat-complex-command)
(define-key global-map [f7] 'query-replace)
;(define-key global-map [f8] 'man)
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
;;;   Code is similar to that in .xemacs file.

(defun my-win32-setup ()

  ;; Under WinNT, to set the default font you have to edit the registry!
  ;; 	HKEY_CURRENT_USER/Software/GNU/Emacs/Emacs.Font
  ;; Here is a nice tiny font:
  ;;	"-*-Terminal-normal-r-*-*-8-60-*-*-c-*-*-oem-"

  ;; Start the Windows server process.
  (if (load "gnuserv" 'noerror 'nomessage)
      (gnuserv-start))

  )

;;;---------------------------------------------------------------------------

(defun my-xwindow-setup ()

  (setq default-frame-alist '((minibuffer . nil)
			      (font . "6x13")
			      (width  . 80)
			      (height . 50)
			      (paneFont . "9x15")
			      (selectionFont . "9x15")))

  (setq minibuffer-frame-alist '((font . "8x13")
				 (width  . 96)
				 (height . 1)
				 (autoraise . t)))

  (setq initial-frame-alist '((minibuffer . nil)
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
      (global-set-key "\C-x\C-c" nil)



      ;; Turn on font-lock mode all over the place!  I like it!
      ;; 19991004: Maybe can simplify this; see .emacs20

      (require 'font-lock)
      (setq font-lock-global-mode t)
;     (setq font-lock-global-mode '(c++-mode html-mode emacs-lisp-mode sgml-mode c-mode lisp-mode tex-mode texinfo-mode dired-mode perl-mode))

;      (copy-face 'default 'font-lock-string-face)
;      (copy-face 'default 'font-lock-function-name-face)
;      (copy-face 'default 'font-lock-comment-face)
;      (set-face-foreground 'font-lock-function-name-face "blue")
;      (set-face-foreground 'font-lock-comment-face "red")
;      (set-face-foreground 'font-lock-string-face "forest green")
;      (set-face-underline-p 'font-lock-string-face nil)

      (add-hook 'emacs-lisp-mode-hook	'turn-on-font-lock)
      (add-hook 'lisp-mode-hook		'turn-on-font-lock)
      (add-hook 'c-mode-hook		'turn-on-font-lock)
      (add-hook 'c++-mode-hook		'turn-on-font-lock)
      (add-hook 'sgml-mode-hook	        'turn-on-font-lock)
      (add-hook 'tex-mode-hook	        'turn-on-font-lock)
      (add-hook 'texinfo-mode-hook	'turn-on-font-lock)
      (add-hook 'dired-mode-hook	'turn-on-font-lock)
      (add-hook 'perl-mode-hook		'turn-on-font-lock)

      (cond 
       ((eq window-system 'x) (my-xwindow-setup))
       ((eq window-system 'w32) (my-win32-setup))
       ))
  ))



;; EOF ;;
