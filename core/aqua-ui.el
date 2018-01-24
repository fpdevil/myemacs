;;; package --- customized settings under aqua-ui.el
;;;
;;; Commentary:
;;;
;;; filename.  : aqua-ui.el
;;; description: contains general Emacs UI based customization's
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;-----------------------------------------------------------------------------
;; frame title to show either a file or a buffer name
;;-----------------------------------------------------------------------------
(setq frame-title-format
      '("" invocation-name " Emacs - " (:eval (if (buffer-file-name)
                                                 (abbreviate-file-name (buffer-file-name))
                                               "%b"))))

;;-----------------------------------------------------------------------------
;; parentheses show
;;-----------------------------------------------------------------------------
(show-paren-mode t)
(setq show-paren-style 'parenthesis)    ;; highlight brackets
(setq show-paren-style 'expression)     ;; highlight entire expression
(setq show-paren-style 'mixed)          ;; highlight brackets if visible, else entire expression

;;-----------------------------------------------------------------------------
;; get visual indication of an exception
;;-----------------------------------------------------------------------------
(set 'visible-bell t)

;;-----------------------------------------------------------------------------
;; syntax highlighting everywhere                                          ;;
;;-----------------------------------------------------------------------------
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

;;-----------------------------------------------------------------------------
;; add proper word wrapping
;;-----------------------------------------------------------------------------
(global-visual-line-mode t)

;;-----------------------------------------------------------------------------
;; additional mode line settings
;;-----------------------------------------------------------------------------
(line-number-mode 1)
(column-number-mode t)
(size-indication-mode t)

;;-----------------------------------------------------------------------------
;; for pdf viewing
;;-----------------------------------------------------------------------------
(setq doc-view-continuous t)

;;-----------------------------------------------------------------------------
;; set default font for aquamacs
;;-----------------------------------------------------------------------------
(when (equal system-type 'darwin)
  (set-frame-font "Monaco-12.0"))
(if (string-match "apple-darwin" system-configuration)
    (set-face-font 'default "Monaco-12"))

;; for gui
(when (display-graphic-p)
  ;; (setq-default mac-option-modifier 'super)
  ;; (setq-default mac-pass-command-to-system nil)
  ;; (set-face-attribute 'default nil :font "DejaVu Sans-12")
  ;; (set-face-attribute 'default nil :font "Inconsolata-13")
  ;; specify a unicode font
  (set-fontset-font "fontset-default"
                    'unicode
                    "-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1"))

(when (boundp 'aquamacs-version)
  (aquamacs-autoface-mode 0)
  (setq mac-allow-anti-aliasing t)                           ;; anti-aliasing
  (setq ns-use-srgb-colorspace t)
  (set-face-bold 'bold nil)                                  ;; disable bold fonts
  ;; font settings
  (set-face-attribute 'default nil
                      :family "Monaco"
                      :width 'normal
                      :height 115
                      :weight 'light)

  ;;
  ;;(set-face-attribute 'default nil
  ;;                    :family "Monaco for Powerline"
  ;;                    :width 'normal
  ;;                    :height 120
  ;;                    :weight 'ultralight)
)

;; customzation for faces
(make-face 'font-lock-number-face)
(set-face-attribute 'font-lock-number-face nil :inherit font-lock-constant-face)
(setq font-lock-number-face 'font-lock-number-face)
(defvar font-lock-number "[0-9-.]+\\([eE][+-]?[0-9]*\\)?")
(defvar font-lock-hexnumber "0[xX][0-9a-fA-F]+")
(defun add-font-lock-numbers (mode)
  (font-lock-add-keywords
   mode
   `((,(concat "\\<\\(" font-lock-number "\\)\\>" ) 0 font-lock-number-face)
     (,(concat "\\<\\(" font-lock-hexnumber "\\)\\>" ) 0 font-lock-number-face))))

;;-----------------------------------------------------------------------------
;; change the starup message in the echo area
;;-----------------------------------------------------------------------------
(defun display-startup-echo-area-message ()
  "Startup echo message."
  (message "Let the hacking begin!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aqua-ui)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; aqua-ui.el ends here
