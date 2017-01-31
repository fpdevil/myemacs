;;; package  --- delighted-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : delighted-config.el
;;; Description: Emacs customization of the mode(s) displayed
;;;              using the delight & dim plugins from elpa.
;;;
;;; symbols can be checked with (C-h v minor-mode-alist)
;;;===========================================================================
(require 'delight)  ;; customise how major and minor modes appear
(require 'dim)      ;; customize mode-line names of major/minor modes
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delight - for customizing the mode names displayed on modeline           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (delight '(
;            ;; (company-mode " COMPANY" company)
;            (company-mode " Ⓐ" company)
;            ;; (hs-minor-mode " ⓗ" hideshow)
;            ;; (outline-minor-mode " Ⓞ" outline)
;            (outline-mode " Ⓞ" :major)
;            (git-gutter-mode " Ⓖ" git-gutter)
;            ;; (flyspell-mode " Ⓕ" flyspell)
;            ;; (smartparens-mode " Ⓢ" smartparens)
;            ;; (elisp-slime-nav-mode nil elisp-slime-nav)
;            ;; (ess-noweb-font-lock-mode nil ess)
;            ;; (reftex-mode " Ⓡ" reftex)
;            ;; (ess-noweb-mode " Ⓝ" ess)
;            ;; (anzu-mode " Ⓩ" anzu)
;            ;; (abbrev-mode " ⓐ" abbrev)
;            ;; (helm-mode " Ⓗ" helm)
;            (flymake-mode " FlyM" flymake)
;            ;; (jedi-mode " Jedi" jedi)
;            (visual-line-mode " Ⓥ" simple)
;            (yas-minor-mode " Ⓨ" yasnippet)
;            (which-key-mode " Ⓚ" which-key)
;            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dim - for customizing the mode names displayed on modeline               ;;
;; https://github.com/alezost/dim.el                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dim-major-names
 '((outline-mode        " Ⓞ")
   (calendar-mode       " 📆")))
(dim-minor-names
 '((company-mode        " COM" company)
   (flymake-mode        " FlyM" flymake)
   (flyspell-mode       " FlyS" flyspell)
   (git-gutter-mode     " Ⓖ" git-gutter)
   (visual-line-mode    " Ⓥ")
   (yas-minor-mode      " Ⓨ")
   (auto-fill-function  " ℱ")
   (which-key-mode      " Ⓚ")
   (eldoc-mode          " Ⓔ" eldoc)
   (whitespace-mode     " {W}"  whitespace)
	 (undo-tree-mode      " 🌴")
   (projectile-mode     " π")
   (paredit-mode        " {Ƥ}" paredit)))


(provide 'delighted-config)
;;; delighted-config.el ends here
