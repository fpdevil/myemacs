;;; package  --- diminish-config.el
;;;
;;; Commentary:
;;; Diminished modes are minor modes with no modeline display
;;; hide a minor mode that you know are always enabled using this
;;; http://www.eskimo.com/~seldon/diminish.el
;;;
;;; Filename   : diminish-config.el
;;; Description: Emacs customization of the mode(s) displayed
;;;              using the delight & dim plugins from elpa.
;;;
;;; symbols can be checked with (C-h v minor-mode-alist)
;;;
;;; Code:
;;;
;;;============================================================================
(require 'dim)      ;; customize mode-line names of major/minor modes
(require 'diminish) ;; diminish minor mode displays

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dim - for customizing the mode names displayed on modeline               ;;
;; https://github.com/alezost/dim.el                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dim-major-names '(
                    (outline-mode             " Ⓞ")
                    (calendar-mode            " 📆")))
(dim-minor-names '(
                   (company-mode             " ℂ" company)
                    (git-gutter-mode          " Ⓖ" git-gutter)
                    (visual-line-mode         " Ⓥ")
                    (yas-minor-mode           " Ⓨ")
                    (auto-fill-function       " ℱ")
                    ;;(undo-tree-mode           " ፕ")
                    (paredit-mode             " {ק}" paredit)
                    (helm-mode                " Ⓗ")
                    (rainbow-mode             " 🌈")))

;;----------------------------------------------------------------------------
;; diminish unneeded minor modes from mode line
;;----------------------------------------------------------------------------
(diminish 'visual-line-mode)
(after 'which-key (diminish 'which-key-mode))
(after 'guide-key (diminish 'guide-key-mode))
(after 'whitespace (diminish 'whitespace-mode))
(after 'undo-tree (diminish 'undo-tree-mode))
(after 'volatile-highlights (diminish 'volatile-highlights-mode))
(after 'golden-ratio (diminish 'golden-ratio-mode))
(after 'eldoc (diminish 'eldoc-mode))
(after 'abbrev (diminish 'abbrev-mode))
(after 'smartparens (diminish 'smartparens-mode))
(after 'evil-smartparens (diminish 'evil-smartparens-mode))
(after 'elisp-slime-nav (diminish 'elisp-slime-nav-mode))
(after 'git-gutter+ (diminish 'git-gutter+-mode))
(after 'highlight-symbol (diminish 'highlight-symbol-mode))
(after 'highlight-indentation (diminish 'highlight-indentation-mode))
(after 'indent-guide (diminish 'indent-guide-mode))
(after 'hideshow (diminish 'hs-minor-mode))
(after 'evil-commentary (diminish 'evil-commentary-mode))
(after 'page-break-lines (diminish 'page-break-lines-mode))
(after 'counsel (diminish #'counsel-mode))
(after 'rich-minority-mode (diminish 'rich-minority-mode))

;;----------------------------------------------------------------------------

(provide 'diminish-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; delighted-config.el ends here
