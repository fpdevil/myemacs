;;; package  --- delighted-config.el
;;;
;;; Commentary:
;;; Diminished modes are minor modes with no modeline display
;;; hide a minor mode that you know are always enabled using this
;;; http://www.eskimo.com/~seldon/diminish.el
;;;
;;; Filename   : delighted-config.el
;;; Description: Emacs customization of the mode(s) displayed
;;;              using the delight & dim plugins from elpa.
;;;
;;; symbols can be checked with (C-h v minor-mode-alist)
;;;===========================================================================
;(require 'delight)  ;; customise how major and minor modes appear
(require 'dim)      ;; customize mode-line names of major/minor modes
(require 'diminish) ;; diminish minor mode displays
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
;            ;; (git-gutter-mode " Ⓖ" git-gutter)
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
(dim-major-names '(
                    (outline-mode             " Ⓞ")
                    (calendar-mode            " 📆")))
(dim-minor-names '(
                    (flymake-mode             " 𝔽𝕝𝕪𝕄" flymake)
                    (flyspell-mode            " 𝔽𝕝𝕪𝕊" flyspell)
                    (company-mode             " ℂ𝕆" company)
                    (git-gutter-mode          " Ⓖ" git-gutter)
                    (visual-line-mode         " Ⓥ")
                    (yas-minor-mode           " Ⓨ")
                    (interactive-haskell-mode " 𝕴")
                    (auto-fill-function       " ℱ")
                    (which-key-mode           " Ⓚ")
                    (whitespace-mode          " 𝑾"  whitespace)
                    (undo-tree-mode           " ፕ")
                    (projectile-mode          " 𝓟")
                    (paredit-mode             " {ק}" paredit)
                    (emacs-lisp-mode          " ε")
                    (helm-mode                " Ⓗ")
                    (rainbow-mode             " 🌈")
   ))

;;----------------------------------------------------------------------------
;; diminish unneeded minor modes from mode line
;;----------------------------------------------------------------------------
(eval-after-load "guide-key"
  '(diminish 'guide-key-mode))            ;; hide the Guide from mode line
(eval-after-load "highlight-symbol"
  '(diminish 'highlight-symbol-mode))     ;; hide the hl-s from mode line
(eval-after-load "whitespace"
  '(diminish 'whitespace-mode))           ;; whitespace handling
(eval-after-load "eldoc"
  '(diminish 'eldoc-mode))                ;; hide eldoc-mode from mode line
(eval-after-load "abbrev"
  '(diminish 'abbrev-mode))               ;; hide Abbrev
(eval-after-load "golden-ratio"
  '(diminish 'golden-ratio-mode))         ;; Hide Golden
(eval-after-load "editorconfig"
  '(diminish 'editorconfig-mode))         ;; Hide editorconfig
(eval-after-load "volatile-highlights"
  '(diminish 'volatile-highlights-mode))  ;; Hide volatile-highlights

;;----------------------------------------------------------------------------

(provide 'delighted-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; delighted-config.el ends here
