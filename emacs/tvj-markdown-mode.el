
; http://jblevins.org/projects/markdown-mode/
(load "markdown-mode")


(defun tvj-markdown-custom ()
  "Todd's markdown-mode-hook"

  ;; I want underscore to mean italics
  (setq markdown-italic-underscore t)
  (auto-fill-mode 1))

(add-hook 'markdown-mode-hook '(lambda() (tvj-markdown-custom)))
