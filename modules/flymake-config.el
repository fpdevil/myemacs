;;; package  --- flymake-config.el
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename.  : flymake-config.el
;;; Description: configuration file for flymake error checker.
;;;              FlyMake performs on-the-fly syntax checks on the files being
;;;              edited using the external syntax check tool (usually the compiler).
;;;
;;; syntax checking for GNU Emacs - https://www.emacswiki.org/emacs/FlyMake
;;;===========================================================================
(require 'flymake)
(require 'flymake-easy)               ;; helpers for easily building Emacs flymake checkers
(require 'flymake-cursor)             ;; show flymake errors in mini buffer


(provide 'flymake-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; flymake-config.el ends here
