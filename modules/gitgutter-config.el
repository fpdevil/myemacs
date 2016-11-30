;;; package  --- gitgutter-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : gitgutter-config.el
;;; Description: Emacs port of GitGutter which is Sublime Text Plugin
;;;              configuration file for custom settings.
;;;              https://github.com/syohex/emacs-git-gutter
;;;
;;; elisp code for customizing the git-gutter settings
;;;==========================================================================
(require 'git-gutter)
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs port of GitGutter (git integration)                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If you enable global minor mode
(global-git-gutter-mode t)
;; (custom-set-variables
;;  '(git-gutter:modified-sign "  ")                   ;; two space
;;  '(git-gutter:added-sign "++")                      ;; multiple character is OK
;;  '(git-gutter:deleted-sign "--"))

(set-face-background 'git-gutter:modified "purple")    ;; background color
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")

(provide 'gitgutter-config)

;;; gitgutter-config.el ends here
