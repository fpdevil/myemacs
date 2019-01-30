;;; package --- customize highlight symbols configuration for Emacs
;;;
;;; Commentary:
;;; Filename   : highlight-symbol-config.el
;;; description: elisp code for customizing highlight symbol for Emacs
;;;
;;; If you move point on a symbol, it will automatically highlights all the
;;; symbols in the current screen.  From now on, pressing M-n and M-p will
;;; immediately jump to the next/previous symbols in a buffer.
;;;
;;; Code:
;;;

(require 'highlight-symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; automatic and manual symbol highlighting for Emacs                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
  (add-hook hook 'highlight-symbol-mode)
  (add-hook hook 'highlight-symbol-nav-mode))
(add-hook 'org-mode-hook (lambda () (highlight-symbol-nav-mode)))

(setq highlight-symbol-idle-delay 0.3
      highlight-symbol-on-navigation-p t)

(global-set-key [(control shift mouse-1)]
                (lambda (event)
                  (interactive "e")
                  (goto-char (posn-point (event-start event)))
                  (highlight-symbol-at-point)))

(global-set-key (kbd "M-n") 'highlight-symbol-next)
(global-set-key (kbd "M-p") 'highlight-symbol-prev)

;;**
;; highlight symbols with keymap-enabled overlays
(require-package 'symbol-overlay)
(after 'symbol-overlay
  (add-hook 'prog-mode-hook #'symbol-overlay-mode)
  (add-hook 'markdown-mode-hook #'symbol-overlay-mode)
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
  (define-key symbol-overlay-map (kbd "p") 'symbol-overlay-jump-prev) ;; 次のシンボルへ
  (define-key symbol-overlay-map (kbd "n") 'symbol-overlay-jump-next) ;; 前のシンボルへ
  (define-key symbol-overlay-map (kbd "C-g") 'symbol-overlay-remove-all))



(provide 'highlight-symbol-config)

;;; highlight-symbol-config.el ends here
