;;; Basic .emacs file.
;;;
;;; DO NOT place "useful" things in here.  All of that should go into the file
;;; dotemacs.el in the emacs library directory.
;------------------------------------------------------------------------------

;; Might want to modify this for some environments.
(setq my-etc-dir (expand-file-name "~/etc/"))

(setq my-emacs-dir (concat my-etc-dir "emacs/"))
(setq load-path (cons my-emacs-dir load-path))


;; Here's where all the real action takes place:
(load "dotemacs")

;; Custom will edit this file with manually configured settings.

