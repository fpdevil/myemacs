;;; package  --- gitgutter-config.el
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : gitgutter-config.el
;;; Description: Emacs port of GitGutter which is Sublime Text Plugin
;;;              configuration file for custom settings.
;;;              https://github.com/syohex/emacs-git-gutter
;;;
;;; elisp code for customizing the git-gutter settings
;;; https://learngitbranching.js.org/
;;;
;;; Code:
;;;

(require 'git-gutter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs port of GitGutter (git integration)                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If you enable global minor mode
(global-git-gutter-mode t)
;; (custom-set-variables
;;  '(git-gutter:modified-sign "  ")                   ;; two space
;;  '(git-gutter:added-sign "++")                      ;; multiple character is OK
;;  '(git-gutter:deleted-sign "--"))
(custom-set-variables
 '(git-gutter:window-width 2)
 '(git-gutter:modified-sign "☁")
 '(git-gutter:added-sign "☀")
 '(git-gutter:deleted-sign "☂"))


;(set-face-background 'git-gutter:modified "purple")    ;; background color
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")

(provide 'gitgutter-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; gitgutter-config.el ends here
