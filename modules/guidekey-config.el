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
;; (setq guide-key/guide-key-sequence '("C-x"))
(setq guide-key/guide-key-sequence '("C-x" "C-c" "C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n" "C-x C-r" "C-x r" "M-s" "C-h"))
(setq guide-key/recursive-key-sequence-flag t)

;; some specific settings for org-mode
(defun guide-key/my-hook-function-for-org-mode ()
  "Org mode specific settings for guide key."
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-highlight-command-regexp "org-"))
(add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)

;; guide-key tip
;; (setq guide-key-tip/enabled t)

(provide 'guidekey-config)
;;; guidekey-config.el ends here
