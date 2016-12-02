;;; package  --- delight-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : delight-config.el
;;; Description: Emacs customization of the mode(s) displayed
;;;              using the delight plugin from elpa.
;;;
;;; elisp code for customizing the modeline display settings
;;;===========================================================================
(require 'delight) ;; customise how major and minor modes appear
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delight - for customizing the mode names displayed on modeline           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(delight '(
           (company-mode " COMPANY" company)
           ;; (company-mode " Ⓐ" company)
           ;; (hs-minor-mode " ⓗ" hideshow)
           ;; (outline-minor-mode " Ⓞ" outline)
           (outline-mode " Ⓞ" :major)
           (git-gutter-mode " Ⓖ" git-gutter)
           ;; (flyspell-mode " Ⓕ" flyspell)
           ;; (smartparens-mode " Ⓢ" smartparens)
           ;; (elisp-slime-nav-mode nil elisp-slime-nav)
           ;; (ess-noweb-font-lock-mode nil ess)
           ;; (reftex-mode " Ⓡ" reftex)
           ;; (ess-noweb-mode " Ⓝ" ess)
           ;; (anzu-mode " Ⓩ" anzu)
           ;; (abbrev-mode " ⓐ" abbrev)
           ;; (helm-mode " Ⓗ" helm)
           (flymake-mode " FlyM" flymake)
           ;; (jedi-mode " Jedi" jedi)
           (visual-line-mode " Ⓥ" simple)
           (yas-minor-mode " Ⓨ" yasnippet)
           (which-key-mode " Ⓚ" which-key)
           ))


(provide 'delight-config)

;;; delight-config.el ends here
