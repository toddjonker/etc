;;; JDE Customizations


(setq load-path (cons (concat my-libemacs-dir "/jde-2.1.5")
		      load-path))
(require 'jde)



(defconst todd-Java-defun-prompt-regexp
  "^[ \t]*\\(\\(\\(public\\|protected\\|private\\|const\\|abstract\\|synchronized\\|final\\|static\\|threadsafe\\|transient\\|native\\|volatile\\)\\s-+\\)*\\(\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*[][_$.a-zA-Z0-9]+\\|[[a-zA-Z]\\)\\s-*\\)\\s-+\\)\\)?\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*\\s-+\\)\\s-*\\)?\\([_a-zA-Z][^][ \t:;.,{}()=]*\\|\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)\\)\\s-*\\(([^);{}]*)\\)?\\([] \t]*\\)\\(\\s-*\\<throws\\>\\s-*\\(\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)[, \t\n\r\f]*\\)+\\)?\\s-*")




(defun my-jde-mode-hook ()
  ;; Note that this also inherits everything from my-java-mode-hook.

  ;; Get prev/next defun to work better (but still not correctly).
  '(setq defun-prompt-regexp c-Java-defun-prompt-regexp)

  (todd-jde-make-patterns)
  '(setq defun-prompt-regexp todd-defun-prompt)

  '(define-key jde-mode-map "\M-\C-e" 'todd-next-java-method)

  ;; I hate abbrev mode!
  (setq abbrev-mode nil)

  )
(add-hook 'jde-mode-hook 'my-jde-mode-hook)



(defun todd-jde-gen-upcase-named (name)
  (jde-gen-init-cap (jde-gen-lookup-named name)))


(defcustom todd-jde-gen-property-template
  '(
    "'n>"
    "\"private \" (P \"Variable type: \" type)"
    "(P \"Variable name: \" name 'noinsert)"
    "\" my\" (todd-jde-gen-upcase-named 'name) \";\" 'n> 'n>"
    "\"/**\" 'n>"
    "\"Get the value of the \" (s name) \" property.\" 'n>"
    "\"@return Value of the \" (s name) \" property.\" 'n"
    "\"*/\" '> 'n>"
    "\"public \" (s type) \" get\" (todd-jde-gen-upcase-named 'name)"
    "\"()\" 'n>"
    "\"{\" '> 'n>"
    "\"return my\" (todd-jde-gen-upcase-named 'name) \";\" 'n>"
    "\"}\" '> 'n 'n>"
    "\"/**\" 'n>" "\"Set the value of the \" (s name) \" property.\" 'n>"
    "\"@param v  Value to assign to the \" (s name) \" property.\" 'n>"
    "\"*/\" '> 'n>"
    "\"public void set\" (todd-jde-gen-upcase-named 'name)"
    "\"(\" (s type) \" \" (s name) \")\" 'n"
    "\"{\" '> 'n>"
    "\"my\" (todd-jde-gen-upcase-named 'name) \" = \" (s name) \";\" 'n"
    "\"}\" '> 'n>"
    )
  "*Template for creating a JavaBean property get/set method pair.
Setting this variable defines a template instantiation
command `todd-jde-gen-property', as a side-effect."
  :group 'jde-gen
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'todd-jde-gen-property
	    (tempo-define-template
	     "java-property"
	     (jde-gen-read-template val)
	     nil
	     "Insert JavaBean property get-set method pair."))
	  (set-default sym val)))


;;============================================================================

(defvar todd-defun-prompt "")


(defun todd-jde-make-patterns ()
  "Makes a replacement for the regular expression indexing  
patterns in imenu, which are too slow for the JDE's
speedbar. See `imenu-generic-expression'."
  (let* ((capital "A-Z\300-\326\330-\337")
	 (letter "a-zA-Z_$\300-\326\330-\366\370-\377")
	 (digit "0-9")
	 (white-space "\\s-")
	 (optional-white-spaces
	  (concat white-space "*"))
	 (bol "^")
	 (eol "$")
	 (not-comment (concat optional-white-spaces "[^.*/]*"))
	 (anything ".*")

	 (primitive-type 
	  (concat "\\<\\(b\\(oolean\\|yte\\)"
		  "\\|char\\|double\\|float\\|int"
		  "\\|long\\|short\\|void\\)\\>"))
	 (primitive-type-count 2)

	 (primitive-type-no-void 
	  (concat "\\<\\(b\\(oolean\\|yte\\)"
		  "\\|char\\|double\\|float\\|int"
		  "\\|long\\|short\\)\\>"))
	 (primitive-type-no-void-count 2)

	 (identifier
	  (concat "\\<\\([" letter "][" letter digit "]*\\)\\>"))

	 ;; Class types are assumed to begin with a capital letter.
	 (class-type
	  (concat
	   "\\<\\([" capital "][a-zA-Z_" digit "]*\\)\\>"))

	 (modifier
	  (concat 
	   "\\<\\(abstract\\|const\\|final\\|native\\|"
	   "p\\(r\\(ivate\\|otected\\)\\|ublic\\)\\|"
	   "s\\(tatic\\|ynchronized\\)\\|transient\\|volatile\\)\\>"))

	 (optional-modifiers
	  (concat
	   "\\(" modifier optional-white-spaces "\\)*"))

	 (modifiers
	  (concat
	   "\\(" modifier optional-white-spaces "\\)+"))


	 (optional-array-modifier
	  (concat
	   "\\(\\[" optional-white-spaces "\\]" optional-white-spaces "\\)*"))


	 (class
	  (concat
	   bol
	   optional-white-spaces
	   optional-modifiers
	   "\\<class\\>"
	   optional-white-spaces
	   identifier))

	   
	 (interface
	  (concat
	   bol
	   optional-white-spaces
	   optional-modifiers
	   "\\<interface\\>"
	   optional-white-spaces
	   identifier))


	 (constructor
	  (concat
	   bol
	   optional-white-spaces
	   modifiers                 ;; e.g., public
	   class-type                ;; e.g., Foo
	   optional-white-spaces
	   "("))


	 ;; Pattern for methods that return a primitive type
	 (method1
	  (concat
	   bol
	   not-comment
	   primitive-type          ;; e.g., int
	   optional-white-spaces
	   optional-array-modifier ;; e.g., []
	   identifier              ;; e.g., foo
	   optional-white-spaces
	   "("))
	
	 ;; Pattern for methods that return a class type
	 (method2
	  (concat
	   bol
	   not-comment
	   class-type
	   optional-white-spaces
	   optional-array-modifier
	   identifier
	   optional-white-spaces
	   "("))


	 (variable1
	  (concat
	   bol
	   optional-white-spaces
	   optional-modifiers
	   optional-white-spaces
	   class-type
	   optional-white-spaces
	   optional-array-modifier
	   optional-white-spaces
	   identifier
	   optional-white-spaces
	   "\\(;\\|=\\)"))


	 (variable2
	  (concat
	   bol
	   optional-white-spaces
	   optional-modifiers
	   optional-white-spaces
	   primitive-type-no-void
	   optional-white-spaces
	   optional-array-modifier
	   optional-white-spaces
	   identifier
	   optional-white-spaces
	   "\\(;\\|=\\)"))


	 (any-type
	  (concat
	   "\\(" primitive-type "\\|" class-type "\\)"
	   ))

	 (method-decl
	  (concat
	   bol
	   not-comment
	   any-type				;; e.g., int
	   optional-white-spaces
	   optional-array-modifier 		;; e.g., []
	   identifier				;; e.g., foo
	   optional-white-spaces
	   "\\(([^);{}]*)\\)?\\([] \t]*\\)\\("
	   optional-white-spaces
	   "\\<throws\\>\\s-*\\(\\("
	   identifier "\\)[, \t\n\r\f]*\\)+\\)?"
	   optional-white-spaces
	   ))

	 )
    (setq todd-defun-prompt method-decl)))

(todd-jde-make-patterns)

todd-defun-prompt

'(defun todd-next-java-method ()
  (interactive)
  (re-search-forward (concat todd-defun-prompt "{") nil t))

(defun todd-next-java-method ()
  (interactive)
  (beginning-of-defun -1))


(define-key jde-mode-map "\M-\C-e" 'todd-next-java-method)
(define-key jde-mode-map "\M-\C-e" 'end-of-defun)

;;; EOF ;;;
