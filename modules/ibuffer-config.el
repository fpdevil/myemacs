;;; package  --- ibuffer-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : ibuffer-config.el
;;; Description: Miscellaneous configuration and customization for Emacs
;;;              elisp code snippets for customizing Emacs
;;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; -- ibuffer More of the mixed up stuff
(require 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired"      (mode . dired-mode))
               ("java"       (mode . java-mode))
               ("erlang"     (mode . erlang-mode))
               ("haskell"    (mode . haskell-mode))
               ("javascript" (mode . js-mode))
               ("python"     (mode . python-mode))
               ("org"        (mode . org-mode))
               ("elisp"      (mode . elisp-mode))
               ("xml"        (mode . nxml-mode))))))

(setq ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")
            (ibuffer-filter-by-filename "."))) ;; to show only dired and files buffers

(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; Modify the default ibuffer-formats
  (setq ibuffer-formats
   '((mark modified read-only " "
      (name 18 18 :left :elide)
      " "
      (size-h 9 -1 :right)
      " "
      (mode 16 16 :left :elide)
      " "
      filename-and-process)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; end of ibuffer settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ibuffer-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; misc-config.el ends here