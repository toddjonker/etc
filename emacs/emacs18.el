;;;   todd's .emacs stuff for Emacs 18			-*-emacs-lisp-*-
;;;


;; This function exists in Emacs 19+

(defun toggle-read-only ()
  (interactive)
  (setq buffer-read-only (not buffer-read-only))
  (message (if buffer-read-only "Buffer is read-only." "Buffer is editable.")))





;;;
;;;   Key bindings
;;;

(global-unset-key "\e[")

(define-key global-map "\e[11~" 'find-file)			; F1
(define-key global-map "\e[12~" 'find-file)			; F2
(define-key global-map "\e[13~" 'revert-buffer)			; F3
(define-key global-map "\e[14~" 'bury-buffer)			; F4
(define-key global-map "\e[15~" 'rmail)				; F5

(define-key global-map "\e[17~" 'repeat-complex-command)	; F6
(define-key global-map "\e[18~" 'query-replace)			; F7
(define-key global-map "\e[19~" 'man)				; F8
(define-key global-map "\e[20~" 'toggle-read-only)		; F9
(define-key global-map "\e[21~" 'toggle-debugger)		; F10

(define-key global-map "\e[23~" 'toggle-truncate)		; F11
(define-key global-map "\e[24~" 'fill-paragraph)		; F12
(define-key global-map "\e[25~" 'glue-lines)			; F13
(define-key global-map "\e[26~" 'fill-paragraph)		; F14


(define-key global-map "\e[31~" 'beginning-of-buffer)		; F17
(define-key global-map "\e[32~" 'end-of-buffer)			; F18
(define-key global-map "\e[33~" 'goto-line)			; F19
(define-key global-map "\e[34~" 'what-line)			; F20

(define-key global-map "\e[1~"	'query-replace)			; "Find" key
(define-key global-map "\e[2~"  'overwrite-mode)		; "Insert" key


;;;   EOF   ;;;
