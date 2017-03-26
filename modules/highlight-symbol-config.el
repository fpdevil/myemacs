;;; package --- customize highlight symbols configuration for Emacs
;;;
;;; Commentary:
;;; Filename   : highlight-symbol-config.el
;;; description: elisp code for customizing highlight symbol for Emacs
;;;
;;; If you move point on a symbol, it will automatically highlights all the
;;; symbols in the current screen.  From now on, pressing M-n and M-p will
;;; immediately jump to the next/previous symbols in a buffer.
;;;===========================================================================
(require 'highlight-symbol)
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; automatic and manual symbol highlighting for Emacs                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
  (add-hook hook 'highlight-symbol-mode)
  (add-hook hook 'highlight-symbol-nav-mode))
(add-hook 'org-mode-hook (lambda () (highlight-symbol-nav-mode)))

(setq highlight-symbol-idle-delay 0.2
      highlight-symbol-on-navigation-p t)

(global-set-key [(control shift mouse-1)]
                (lambda (event)
                  (interactive "e")
                  (goto-char (posn-point (event-start event)))
                  (highlight-symbol-at-point)))

(global-set-key (kbd "M-n") 'highlight-symbol-next)
(global-set-key (kbd "M-p") 'highlight-symbol-prev)

(provide 'highlight-symbol-config)

;;; highlight-symbol-config.el ends here
