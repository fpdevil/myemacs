;;; package  --- diminish-config.el
;;;
;;; Commentary:
;;; Diminished modes are minor modes with no mode line display
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
;;;

(require 'dim)      ;; customize mode-line names of major/minor modes
(require 'diminish) ;; diminish minor mode displays

(defvar list-diminished-modes nil
  "A list of diminished modes to either unicode or ascii values.")

(defmacro diminished-mode-symbol (mode &optional unicode ascii)
  "Diminish MODE name in the mode line to either UNICODE or ASCII based on the support for UNICODE character symbols.  If ASCII is not provided then UNICODE will be used.  If neither of them are provided, mode will not be shown in the mode line."
  (let ((cell (assq ',mode list-diminished-modes)))
    (if cell
        (setcdr cell '(,unicode ,ascii))
      (push '(,mode ,unicode ,ascii) list-diminished-modes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dim - for customizing the mode names displayed on mode line                ;;
;; https://github.com/alezost/dim.el                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dim-major-names '(
                    (outline-mode            " Ⓞ")
                    (calendar-mode           " 📆")))
(dim-minor-names '(
                   (flymake-mode               "FlyM")
                   (flyspell-mode              "FlyS")
                   (company-mode               "Ⓒ" company)
                   (auto-complete-mode         "Ⓐ")
                   (yas-minor-mode             "Ⓨ" yas)
                   (interactive-haskell-mode   "Ⓘ" IntHS)
                   ;;(rainbow-mode               "Ⓡ")
                   ;;(git-gutter-mode            "Ⓖ" git-gutter)
                   ;;(paredit-mode               "{ק}" paredit)
                   ))


;;------------------------------------------------------------------------------
;; diminish unneeded minor modes from mode line
;;------------------------------------------------------------------------------
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
(after 'projectile (diminish 'projectile-mode))
(after 'beacon (diminish 'beacon-mode))
(after 'color-identifiers-mode (diminish 'color-identifiers-mode))
(after 'auto-fill-function (diminish 'auto-fill-mode))
(after 'haskell-interactive-mode (diminish 'interactive-haskell-mode " IntHS"))
(after 'haskell-doc (diminish 'haskell-doc-mode))
(after 'checkdoc (diminish 'checkdoc-minor-mode))
(after 'aggressive-indent (diminish 'aggressive-indent-mode))
(after 'paredit (diminish 'paredit-mode))
(after 'helm-mode (diminish 'helm-mode))
(after 'ivy (diminish 'ivy-mode))
(after 'git-gutter (diminish 'git-gutter-mode))
(after 'yapfify (diminish 'yapf-mode))
(after 'sphinx-doc (diminish 'sphinx-doc-mode))
(after 'rainbow-mode (diminish 'rainbow-mode))
(after 'company-box-mode (diminish 'company-box-mode))
(after 'python-docstring-mode (diminish 'python-docstring-mode))


(provide 'diminish-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; diminish-config.el ends here
