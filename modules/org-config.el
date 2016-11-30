;;; package --- org bullets configuration
;;;
;;; Commentary:
;;;
;;; Filename   : org-config.el
;;; Description: Show bullets in org-mode as UTF-8 characters
;;;              configuration file for org bullets mode
;;;              https://github.com/sabof/org-bullets
;;
;; elisp code for org support and handling
;;;==========================================================================
(require 'org-bullets)
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org bullets for markdown                                                 ;;
;; use org-bullets-mode for utf8 symbols as org bullets                     ;;
;; select, do [M-x eval-region]. The *s will be replaced with utf-8 bullets ;;
;; next time you open an org file                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-bullets-bullet-list
        '("◉" "◎" "⚫" "○" "►" "◇"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
(sequence "⚑ WAITING(w)" "|")
(sequence "|" "✘ CANCELED(c)")))

(provide 'org-config)

;;; org-config.el ends here
