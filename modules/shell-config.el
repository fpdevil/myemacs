;;; package --- shell script handling for Emacs
;;;
;;; Filename   : shell-config.el
;;; description: shell script identify and load for Emacs
;;;
;;; Commentary:
;;             for shell scripts
;;-------------------------------------------------------------------

;;;
;;; Code:
;;;

(add-to-list 'auto-mode-alist '("\\.bash_profile\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash_history\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc.local\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . sh-mode))

(provide 'shell-config)

;;-------------------------------------------------------------------
;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; shell-config.el ends here
