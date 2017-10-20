;;; package --- customized settings under aqua-ui.el
;;;
;;; Commentary:
;;;
;;; filename.  : aqua-ui.el
;;; description: contains general Emacs UI based customization's
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code:
;;;

;----------------------------------------------------------------------------
;; frame title to show either a file or a buffer name
;----------------------------------------------------------------------------
(setq frame-title-format
      '("" invocation-name " Emacs - " (:eval (if (buffer-file-name)
                                                 (abbreviate-file-name (buffer-file-name))
                                               "%b"))))

;----------------------------------------------------------------------------
;; parentheses show
;----------------------------------------------------------------------------
(show-paren-mode t)
(setq show-paren-style 'parenthesis)    ;; highlight brackets
(setq show-paren-style 'expression)     ;; highlight entire expression
(setq show-paren-style 'mixed)          ;; highlight brackets if visible, else entire expression

;----------------------------------------------------------------------------
;; get visual indication of an exception
;----------------------------------------------------------------------------
(set 'visible-bell t)

;----------------------------------------------------------------------------
;; syntax highlighting everywhere                                          ;;
;----------------------------------------------------------------------------
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)


;----------------------------------------------------------------------------
;; add proper word wrapping                                                ;;
;----------------------------------------------------------------------------
(global-visual-line-mode t)


;----------------------------------------------------------------------------
;; additional mode line settings
;----------------------------------------------------------------------------
(line-number-mode 1)
(column-number-mode t)
(size-indication-mode t)


;----------------------------------------------------------------------------
;; for pdf viewing                                                         ;;
;----------------------------------------------------------------------------
(setq doc-view-continuous t)


;----------------------------------------------------------------------------
;; set default font for aquamacs
;----------------------------------------------------------------------------
(if (string-match "apple-darwin" system-configuration)
    (set-face-font 'default "Monaco-12"))

(when (boundp 'aquamacs-version)
  (aquamacs-autoface-mode 0)
  (setq mac-allow-anti-aliasing t)                           ;; anti-aliasing
  (setq ns-use-srgb-colorspace t)
  (set-face-attribute 'default nil
                      :family "Monaco"
                      :width 'normal
                      :height 115
                      :weight 'light)
  (set-face-bold 'bold nil)                                  ;; disable bold fonts
  ;(set-face-bold-p 'bold nil)                               ;; disable bold fonts

  ;;
  ; (set-face-attribute 'default nil :family "Monaco for Powerline"
                                     ;:height 120
                                     ;:weight 'ultralight)
)

;----------------------------------------------------------------------------
;; change the starup message in the echo area
;----------------------------------------------------------------------------
(defun display-startup-echo-area-message ()
  "Startup echo message."
  (message "Let the hacking begin!"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aqua-ui)
;;; aqua-ui.el ends here
