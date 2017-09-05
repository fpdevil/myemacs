;;; package --- visual regexp configuration for Emacs
;;;
;;; Commentary:
;;;            Visual Regular Expression support
;;;
;;; Filename   : vregex-config.el
;;; Description: Visual Regex configuration for Emacs
;;;===========================================================================
;;;
;;;Code:
;;;

(use-package visual-regexp
  :defer t
  :config
  (use-package visual-regexp-steroids
    :ensure
    :demand
    :bind (("C-M-r" . vr/isearch-backward)
           ("C-M-s" . vr/isearch-forward)
           ("C-M-%" . vr/query-replace))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'vregex-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; vregex-config.el ends here
