;;; package  --- guidekey-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : guidekey-config.el
;;; Description: Emacs package that displays available keybindings in popup
;;;              https://github.com/kai2nenobu/guide-key
;;; elisp code for customizing the which-key settings
;;;==========================================================================
(require 'guide-key)
(require 'guide-key-tip)
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs package that displays available keybindings in popup               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(guide-key-mode 1)  ; Enable guide-key-mode

;; (setq guide-key/highlight-command-regexp "rectangle")
(setq guide-key/highlight-command-regexp
      '("rectangle"
        ("register" . font-lock-type-face)
        ("bookmark" . "hot pink")))

;; Check key sequence recursively
(setq guide-key/guide-key-sequence '("C-x"))
(setq guide-key/recursive-key-sequence-flag t)

;; guide-key tip
;; (setq guide-key-tip/enabled t)

(provide 'guidekey-config)
;;; guidekey-config.el ends here
