;;;   todd's .emacs                                     -*-emacs-lisp-*-
;;;
;------------------------------------------------------------------------------


;;; Perhaps only needed for Emacs 22
(defconst inhibit-startup-screen t)


;(load-library "basics")
;(require 'scroll-in-place)

(read-abbrev-file (concat my-emacs-dir "abbrevs"))


; Set up an autoload for syntax decoding stuff.
;(autoload 'decode-syntax-table "syntax-decode" "autoloadable function" t)



;;; TNT: an AOL Instant Messenger client
;(load "tnt")
;(setq tnt-default-username "Todd15202")


(load-library "lisp")



;;; tramp.el, included in newer emacsen, allows one to say:
;;;   C-x C-f /ssh:fischman@onca.integ:/apollo/env/OncaWebsite/
;;; or even
;;;   C-x C-f /onca.integ:/apollo/env/OncaWebsite/
;;; to edit files and directories on remote systems one has access to via ssh
;;; (or rsh or ftp or scp...). To prevent being asked about clearing undo
;;; buffers, do
;;;   (setq undo-outer-limit-function '(lambda (size) t))

(if (require 'tramp)
  (setq tramp-verbose 9))



;;;
;;;   New defaults settings
;;;

;; Don't add blank lines when you move down at the end of a file.
(setq next-line-add-newlines nil)


;; Show the column number in the mode line.
(column-number-mode 1)


;; Enable some handy commands:
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;(setq lpr-switches '("-Phpx"))

(setq truncate-partial-width-windows nil)
(setq-default fill-column 79)

(setq default-major-mode 'text-mode)

'(add-hook 'text-mode-hook 
	  (function
	   (lambda ()
	     (setq indent-line-function 'indent-relative-maybe)
	     ;; I don't like this any more.
	     ;; It tends to screw up config files and such.
	     '(auto-fill-mode 1))))


;; Use 4-character tab stops.
(setq tab-stop-list
      (quote ( 4  8 12 16  20  24  28  32  36  40
              44 48 52 56  60  64  68  72  76  80
              84 88 92 96 100 104 108 112 116 120)))



;;;===========================================================================
;;;
;;;  My special functions
;;;

(defun insert-line (char)
  (interactive "cCharacter to use for line: ")
  (cond 
   ((or (eq major-mode 'c++-mode)
        (eq major-mode 'java-mode))
    (insert "\n//")
    (insert-char char 77))
   ((eq major-mode 'perl-mode)
    (insert "\n#")
    (insert-char char 78))
   ((or (eq major-mode 'sgml-mode)
	(eq major-mode 'html-mode))
    (if (eq char ?-)
        (message "You don't want to do that!")
      (progn (insert "\n<!-- ")
             (insert-char char 70)
             (insert " -->"))))
   (t
    (insert-char char 79)))
  (insert "\n"))

(defun insert-c++-comment-line (char)
  (interactive "cCharacter to use for comment line: ")
  (insert "\n//")
  (insert-char char 76)
  (insert "\n"))

(defun insert-sgml-comment-line (char)
  (interactive "cCharacter to use for comment line: ")
  (insert "\n<!-- ")
  (insert-char char 69)
  (insert " -->\n"))

(defun invert-character ()
  "Invert case of character at point"
  (interactive)
  (let* ((char (char-after (point)))
         (down-char (downcase char)))
    (if char
        (progn (if (eq char down-char)
                   (insert-char (upcase char) 1)
                 (insert-char down-char 1))
               (delete-char 1 nil)
               (backward-char 1)))))

(defun up-20 ()
  "Move the point up 20 lines"
  (interactive)
  (forward-line -20))

(defun down-20 ()
  "Move the point down 20 lines"
  (interactive)
  (forward-line 20))


(defun glue-lines ()
  "Glue the next line onto the end of this one"
  (interactive)
  (end-of-line)
  (delete-char 1)
  (just-one-space))


(defun toggle-debugger ()
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message (if debug-on-error "Debugging on." "Debugging off.")))

(defun toggle-truncate ()
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (message (if truncate-lines "Truncation on." "Truncation off.")))

(defun toggle-tab-width ()
  (interactive)
  (if (eq tab-width 8)
      (setq tab-width 4)
    (if (eq tab-width 4)
        (setq tab-width 8)))
  (message "Tab width is %d" tab-width))


(defun replace-initial (regexp newexp &optional beg end)
  "Replace a regular expression within the region. By Rene."
  (interactive "sReplace initial string: \nsReplace initial string %s with: ")
  (if beg ()
    (progn
      (setq beg (region-beginning))
      (setq end (region-end))))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (replace-regexp (concat "^" (regexp-quote regexp)) newexp nil))))


(defun double-line (arg)
  "Double this line.
With positive prefix arg, inserts ARG copies instead of just one.
With negative prefix arg, copies -ARG lines instead of just one."
  (interactive "p")
  (end-of-line)
  (if (< arg 0)
      (insert
       "\n"
       (buffer-substring
        (save-excursion (forward-line (1+ arg)) (point))
        (save-excursion (end-of-line) (point))))
    ;; else insert ARG copies:
    (let ((string (buffer-substring
                   (save-excursion (beginning-of-line) (point))
                   (point))))
      (while (>= (setq arg (1- arg)) 0)
        (insert "\n" string)))))


(defun which-load-file (file &optional nosuffix)
  "Determines from where FILE is loaded.
Returned is a list of pathnames or nil if FILE can't be found in
load-path.  If optional second arg NOSUFFIX is non-nil, don't try
adding suffixes .elc or .el to the specified name FILE."
  (interactive "sFile: \nP")
  (let ((found-list nil)
        (files (if nosuffix
                   (list file)
                 (list (concat file ".elc") (concat file ".el") file)))
        (search-list load-path))
    (while search-list
      (let ((file-list files))
        (while file-list
          (let ((actual-path (concat (car search-list)
                                     (if (string-match "/$" (car search-list))
                                         ""
                                       "/")
                                     (car file-list))))
            (if (and (file-exists-p actual-path)
                     (not (file-directory-p actual-path)))
                (setq found-list (cons actual-path found-list))))
          (setq file-list (cdr file-list))))
      (setq search-list (cdr search-list)))
    (setq found-list (reverse found-list))
    (if (interactive-p)
        (if found-list
            (with-output-to-temp-buffer "*which*"
              (princ file)
              (princ " found as:\n")
              (mapcar (function (lambda (name) (princ name) (terpri)))
                      found-list))
          (message "%s not found" file))
      found-list)))



(defun buffer-convert-newlines ()
  "Convert DOS <return><newline> sequence to UNIX <newline>"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-string "\r\n" "\n")))


;;;===========================================================================
;;;
;;;   General Key bindings
;;;

(global-set-key "\M-z"          'undo)
(global-set-key "\M-g"          'goto-line)
(global-set-key "\M-\C-g"       'goto-char)
(global-set-key "\M-\C-p"       'up-20)
(global-set-key "\M-\C-n"       'down-20)
(global-set-key "\M-p"          'up-20)
(global-set-key "\M-n"          'down-20)

(global-set-key "\M-#"          'what-line)
(global-set-key "\C-^"          'invert-character)

(global-set-key "\C-xc"         'compile)
(global-set-key "\C-x\""        'double-line)
(global-set-key "\C-x\C-b"      'electric-buffer-list)
(global-set-key "\C-x\C-h"      'electric-command-history)
(global-set-key "\M-s"          'save-buffer)
(global-set-key "\M-\t"         'tab-to-tab-stop)

;(global-set-key "\M-\C-s"      'just-one-space)
(global-set-key "\M-\C-l"       'insert-line)
(global-set-key "\C-cg"         'glue-lines)
(global-set-key "\M-r"          'replace-initial)

;(global-set-key "\M-v"         'scroll-down-in-place)
;(global-set-key "\C-v"         'scroll-up-in-place)



;;;===========================================================================
;;;
;;;   Setup for other modes
;;;


;;;
;;;   Electric modes
;;;

; Electric-buffer-list pops up window to select/manipulate buffers.
(setq electric-buffer-menu-mode-hook
      (function
       (lambda ()
         (local-set-key "e" 'Buffer-menu-execute)
         (my-electric-keys))))


; Make the electric modes more useful by adding searching and copying.
(defun my-electric-keys ()
  "Additional key bindings for electric window modes."
  (local-set-key "\C-s" 'isearch-forward)
  (local-set-key "\C-r" 'isearch-backward)
  (local-set-key "\C-f" 'forward-char)
  (local-set-key "\C-b" 'backward-char)
  (local-set-key "\C-a" 'beginning-of-line)
  (local-set-key "\C-e" 'end-of-line)
  (local-set-key "\M-f" 'forward-word)
  (local-set-key "\M-b" 'backward-word)
  (local-set-key "\C-@" 'set-mark-command)
  (local-set-key "\M-w" 'copy-region-as-kill))


(defun view-if-read-only ()
  "Automatically activates view-mode when visiting read-only files."
  (if buffer-read-only 
      (progn (view-mode 1) 
             (setq view-kill-when-finished t))))
(add-hook 'find-file-hooks 'view-if-read-only)



(defun find-file-read-only (filename)
  "Edit file FILENAME but don't save without confirmation.
Like find-file but marks buffer as read-only."
  (interactive "fFind file read-only: ")
  (let ((find-file-hooks (cons '(lambda ()
                                  (setq buffer-read-only t))
                               find-file-hooks)))
    (find-file filename)))



;;;
;;;   Major Editing modes
;;;

;; Get rid of any old c-mode that is still around
;(fmakunbound 'c-mode)
;(makunbound  'c-mode-map)
;(fmakunbound 'c++-mode)
;(makunbound  'c++-mode-map)
;(makunbound  'c-style-alist)

;(autoload 'c++-mode "cc-mode" "C++ Editing Mode" t)
;(autoload 'c-mode   "cc-mode" "C Editing Mode"   t)
(setq auto-mode-alist
      (append '(
		("\\.asdl$" . sgml-mode)
                ("\\.C$"   . c++-mode)
                ("\\.c$"   . c-mode)
                ("\\.cc$"  . c++-mode)
                ("\\.cp$"  . c++-mode)
		("\\.cxx$" . c++-mode)
                ("\\.H$"   . c++-mode)
                ("\\.h$"   . c++-mode)
                ("\\.ih$"  . c++-mode)
		("\\.jelly$" . sgml-mode)
		("\\.jsp$" . java-mode)
		("\\.rvw$" . java-mode)
		("\\.wsdl$" . sgml-mode)
		)
              auto-mode-alist))

(autoload 'beginning-of-code-line "codeline.el"
  "Moves point to first non-whitespace char on line for first invocation.
If line is just white-space, indents to proper column.  Second invocation
moves point to beginning of line.")

(autoload 'end-of-code-line "codeline.el"
  "If line is just a comment, moves to end of line.  Otherwise, on
first invocation moves point to end of code line.  Second invocation
moves point to end of line.")



;; Configuration of CC Mode (base mode for Java, C++, C, etc.)

(setq-default c-basic-offset 4)

(defconst my-c-style
  '("PERSONAL"
    (c-tab-always-indent . nil)
    (c-echo-semantic-information-p . t)
    (c-block-comments-indent-p . t)
    (c-offsets-alist . ((innamespace . 0)
                        (label . 0)
                        (case-label . 4)
			(topmost-intro-cont . 4)
                        (member-init-intro . *)
                        (inher-intro . +)
                        (inline-open . 0)
                        (substatement-open . 0)))
    )
  "My Java/C++/C indentation style")


(defun my-c-mode-hook ()

  ;; Set up my preferred indentation style, but only do it once.
  (let ((my-style-name (car my-c-style)))
    (or (assoc my-style-name c-style-alist)
        (setq c-style-alist (cons my-c-style c-style-alist)))
    (c-set-style my-style-name))

  ;; Display the current line number in the mode line.
  (line-number-mode 1)

  ;; These binding affect c-mode and c++-mode, but not java-mode.
  (define-key c-mode-map "\C-a" 'beginning-of-code-line)
  (define-key c-mode-map "\C-e" 'end-of-code-line)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-hook)


;; Customizations for java-mode

(defun untabify-buffer ()
  (untabify (point-min) (point-max)))


(defun my-java-mode-hook ()

  (setq tab-width 4)

  ;; When we load a java file, retabify it so we don't have to mess with
  ;; tons of spaces. (Remove quote to activate.)
  '(tabify (point-min) (point-max))

  ;; When we write the file, turn all tabs to spaces.
  (add-hook 'write-contents-hooks 'untabify-buffer nil nil)

  (define-key java-mode-map [f2]   'c-indent-exp)
  (define-key java-mode-map "\C-a" 'beginning-of-code-line)
  (define-key java-mode-map "\C-e" 'end-of-code-line)
  )
(add-hook 'java-mode-hook 'my-java-mode-hook)



;; Customizations for jde-mode         http://sunsite.auc.dk/jde/

'(load "todd-jde.el")



;; HTML Helper Mode

;(autoload 'html-helper-mode     "html-helper-mode" "Yay HTML" t)
;(autoload 'jsp-html-helper-mode "html-helper-mode" "Yay HTML" t)
'(setq auto-mode-alist
      (append '(("\\.jhtml$" . html-helper-mode)
		("\\.html$" . html-helper-mode)
		("\\.jsp$"  . jsp-html-helper-mode))
	      auto-mode-alist))

(defun my-html-helper-mode-hook ()
  (setq tab-width 4)
  (setq html-helper-do-write-file-hooks t)

  (setq html-helper-item-basic-offset 2)
  (setq html-helper-item-continue-indent 2)

  ;; When we write the file, turn all tabs to spaces.
  (add-hook 'write-contents-hooks 'untabify-buffer nil nil)
  )
(add-hook 'html-helper-mode-hook 'my-html-helper-mode-hook)


(defun my-xml-mode-hook ()
  ;; When we write the file, turn all tabs to spaces.
  (add-hook 'write-contents-hooks 'untabify-buffer nil nil))
(add-hook 'sgml-mode-hook 'my-xml-mode-hook)


;;; Autoinsert
(load "autoinsert")
(setq auto-insert-directory (concat my-etc-dir "templates/"))
(setq auto-insert-alist '((".*\\.rvw$"   . "CodingTemplate.rvw")
			  (".*\\.java$"  . "CodingTemplate.java")
			  (".*\\.h$"     . "banner.h")
                          ("\\.cxx$"     . "banner.cxx")
                          ("\\.cpp$"     . "banner.cxx")
                          ("\\.c$"       . "banner.cxx")
                          ("[Mm]akefile" . "makefile.inc")
                          ("\\.tex$"     . "tex-insert.tex")
                          ("\\.bib$"     . "tex-insert.tex")
                          ("\\.l$"       . "l-insert.l")))
(add-hook 'find-file-hooks 'auto-insert)


;;;===========================================================================
;;;
;;;   Now load specific stuff depending on what flavor of Emacs 
;;;   we are actually running.
;;;

(if (boundp 'emacs-major-version)
  (if (or (= emacs-major-version 22) (= emacs-major-version 23))
    (load "emacs22")
    (if (or (= emacs-major-version 20) (= emacs-major-version 21))
      (load "emacs20")
      (if (boundp 'pop-up-frames)
	(load "emacs19")
	(load "emacs18")))))
