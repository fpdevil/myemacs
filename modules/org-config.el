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
(require 'org)
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
(setq org-bullets-face-name
      (quote org-bullet-face))
(setq org-bullets-bullet-list
        '("◉" "◎" "⚫" "○" "►" "◇"))
(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode 1)))
(setq org-todo-keywords
      '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
        (sequence "⚑ WAITING(w)" "|")
        (sequence "|" "✘ CANCELED(c)" "▼ DELEGATED(l)" "§ SOMEDAY(f)")))


;;
;; font coloring in code blocks
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-todo-keyword-faces
      '(("IDEA" . (:foreground "GoldenRod" :weight bold))
        ("NEXT" . (:foreground "IndianRed1" :weight bold))
        ("STARTED" . (:foreground "OrangeRed" :weight bold))
        ("WAITING" . (:foreground "IndianRed1" :weight bold))
        ("CANCELED" . (:foreground "LimeGreen" :weight bold))
        ("DELEGATED" . (:foreground "LimeGreen" :weight bold))
        ("SOMEDAY" . (:foreground "LimeGreen" :weight bold))
        ))

(setq org-tag-persistent-alist
      '((:startgroup . nil)
        ("HOME" . ?h)
        ("RESEARCH" . ?r)
        ("TEACHING" . ?t)
        (:endgroup . nil)
        (:startgroup . nil)
        ("OS" . ?o)
        ("DEV" . ?d)
        ("WWW" . ?w)
        (:endgroup . nil)
        ("URGENT" . ?u)
        ("KEY" . ?k)
        ("HARD" . ?a)
        ("BONUS" . ?b)
        ("noexport" . ?x)
        )
      )

(setq org-tag-faces
      '(
        ("HOME" . (:foreground "GoldenRod" :weight bold))
        ("RESEARCH" . (:foreground "GoldenRod" :weight bold))
        ("TEACHING" . (:foreground "GoldenRod" :weight bold))
        ("OS" . (:foreground "IndianRed1" :weight bold))
        ("DEV" . (:foreground "IndianRed1" :weight bold))
        ("WWW" . (:foreground "IndianRed1" :weight bold))
        ("URGENT" . (:foreground "Red" :weight bold))
        ("KEY" . (:foreground "Red" :weight bold))
        ("HARD" . (:foreground "Red" :weight bold))
        ("BONUS" . (:foreground "GoldenRod" :weight bold))
        ("noexport" . (:foreground "Red" :weight bold))
        )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'org-config)

;;; org-config.el ends here
