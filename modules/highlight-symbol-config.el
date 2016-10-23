;;; package --- customize highlight symbols configuration for Emacs
;;;
;;; name: highlight-symbol-config.el
;;; description: highlight symbol configuration for emacs
;
;;; Commentary:
;
; If you move point on a symbol, it automatically highlights all the symbols in
; the current screen. From now on, pressing M-n and M-p will immediately jump to
; the next/previous symbols in a buffer.
;;
;;
; automatic and manual symbol highlighting for Emacs
;;

(require 'highlight-symbol)

(highlight-symbol-nav-mode)

;(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(add-hook 'org-mode-hook (lambda () (highlight-symbol-mode)))

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
;;; python-config ends here
