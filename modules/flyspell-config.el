;;; package  --- flyspell-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : flyspell-config.el
;;; Description: Emacs enable spell checking for comments & text
;;;
;;;===========================================================================
(require 'flyspell)               ; flyspell mode
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flyspell checking for comments                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                haskell-mode-hook
                erlang-mode-hook
                python-mode-hook
                go-mode-hook
                c++-mode-hook
                c-mode-hook
                markdown-mode-hook
                python-mode-hook
                shell-mode-hook
                html-mode-hook
                css-mode-hook
                nxml-mode-hook
                javascript-mode-hook
                LaTeX-mode-hook))
  (add-hook hook 'flyspell-prog-mode))

;; improve performance by not printing messages for every word
(setq flyspell-issue-message-flag nil
      flyspell-issue-welcome-flag nil
      flyspell-use-meta-tab nil)

(provide 'flyspell-config)
;;; flyspell-config.el ends here
