;;; package --- rbow-config
;;;
;;; Commentary:
;;;
;;; Filename   : rbow-config.el
;;; Description: rainbow-mode is a minor mode for Emacs which displays strings
;;;              representing colors with color they represent as background.
;;;
;;; Code:
;;;
;;;===========================================================================
(require 'rainbow-mode)    ;; Colorize color names in buffers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fancy stuff, for colorizing names in buffer with rainbow mode            ;;
;; adds Rbow to the mode line for displaying status                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-hook 'css-mode-hook (lambda () (rainbow-mode 1)))
;; (add-hook 'html-mode-hook (lambda () (rainbow-mode 1)))
;; (add-hook 'prog-mode-hook 'rainbow-mode)

(dolist (hook '(emacs-lisp-mode-hook
                css-mode-hook
                html-mode-hook
                haskell-mode-hook
                erlang-mode-hook
                elixir-mode-hook
                python-mode-hook
                go-mode-hook
                clojure-mode-hook
                js2-mode-hook
                prog-mode-hook))
  (add-hook hook 'rainbow-mode))

(provide 'rbow-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; rbow-config.el ends here
