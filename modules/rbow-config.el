;;; package --- rbow-config
;;;
;;; Commentary:
;;;
;;; Filename   : rbow-config.el
;;; Description: rainbow-mode is a minor mode for Emacs which displays strings
;;;              representing colors with color they represent as background.
;;;===========================================================================
(require 'rainbow-mode)    ;; Colorize color names in buffers
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fancy stuff, for colorizing names in buffer with rainbow mode            ;;
;; adds Rbow to the mode line for displaying status                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'css-mode-hook (lambda () (rainbow-mode 1)))
(add-hook 'html-mode-hook (lambda () (rainbow-mode 1)))
(add-hook 'prog-mode-hook 'rainbow-mode)

(provide 'rbow-config)

;;; rbow-config.el ends here
