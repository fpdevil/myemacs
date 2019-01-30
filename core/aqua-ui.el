;;; package --- customized settings under aqua-ui.el
;;;
;;; Commentary:
;;;
;;; filename.  : aqua-ui.el
;;; description: contains general Emacs UI based customization's
;;;
;;; Code:
;;;

;; * to fix the aquamacs theme setup issue
(setq default-frame-alist nil)

;; * for title bar (OS X emacs 26.1)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

;; *
;; * Display a marker on the left fringe for lines not in the buffer
(setq indicate-empty-lines t)

;; * show no tool bar
(if (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; * show no scroll bar
(if (fboundp 'set-scroll-bar-mode)
    (set-scroll-bar-mode nil))

;; * show no menu bar
;; (if (fboundp 'menu-bar-mode)
;;   (menu-bar-mode -1))

;; *
;;** frame title to show either a file or a buffer name
(setq frame-title-format
      '("" invocation-name " Emacs - " (:eval (if (buffer-file-name)
                                                  (abbreviate-file-name (buffer-file-name))
                                                "%b"))))


;; *
;;** Opening Files within Same Frame, but keep Apple-N functionality the same
(setq one-buffer-one-frame-mode 0)


;; *
;;** Show parentheses
(show-paren-mode t)
(setq show-paren-style 'parenthesis)    ;; highlight brackets
;;(setq show-paren-style 'expression)   ;; highlight entire expression
;;(setq show-paren-style 'mixed)        ;; highlight brackets if visible, else entire expression

;; *
;;** get a visual indication of an exception
(set 'visible-bell t)

;; *
;;** syntax highlighting everywhere
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

;; *
;;**  add a proper word wrapping
;;*** enable visual wordwrap in every text-mode (which includes org-mode)
(global-visual-line-mode)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; *
;;** additional mode line settings
(line-number-mode 1)
(column-number-mode t)
(size-indication-mode t)

;; *
;;** for pdf viewing
(setq doc-view-continuous t)

;; *
;;** set a default font (monaco 12) for Aquamacs
(when (boundp 'aquamacs-version)
  (setq aquamacs-additional-fontsets t))

;;(when (string-equal system-type 'darwin)
;;  (set-frame-font "Monaco-12.0"))
;;(if (string-match "apple-darwin" system-configuration)
;;    (set-face-font 'default "Monaco-12"))

;;** for gui display
;;;(when (display-graphic-p)
;;;  ;; (setq-default mac-option-modifier 'super)
;;;  ;; (setq-default mac-pass-command-to-system nil)
;;;  ;; (set-face-attribute 'default nil :font "DejaVu Sans-12")
;;;  ;; (set-face-attribute 'default nil :font "Inconsolata-13")
;;;  ;; specify a unicode font
;;;  (set-fontset-font "fontset-default"
;;;                    'unicode
;;;                    ;;"-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1"
;;;                    "-*-Monaco-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"
;;))

;;;(when (boundp 'aquamacs-version)
;;;  (aquamacs-autoface-mode 0)
;;;  (setq mac-allow-anti-aliasing t)                           ;; anti-aliasing
;;;  (setq ns-use-srgb-colorspace nil)
;;;  (set-face-bold 'bold nil)                                  ;; disable bold fonts
;;;  ;; font settings
;;;  (set-face-attribute 'default nil
;;;                      :family "Monaco"
;;;                      :width 'normal
;;;                      :height 115
;;;                      :weight 'light)
;;;
;;;  ;;
;;;  ;;(set-face-attribute 'default nil
;;;  ;;                    :family "Monaco for Powerline"
;;;  ;;                    :width 'normal
;;;  ;;                    :height 120
;;;  ;;                    :weight 'ultralight)
;;;)

;; *
;;** customzation for faces
(make-face 'font-lock-number-face)
(set-face-attribute 'font-lock-number-face nil :inherit font-lock-constant-face)

(setq font-lock-number-face 'font-lock-number-face)

(defvar font-lock-number "[0-9-.]+\\([eE][+-]?[0-9]*\\)?")
(defvar font-lock-hexnumber "0[xX][0-9a-fA-F]+")

(defun add-font-lock-numbers (mode)
  "Add font lock numbers to the specified MODE."
  (font-lock-add-keywords
   mode
   `((,(concat "\\<\\(" font-lock-number "\\)\\>" ) 0 font-lock-number-face)
     (,(concat "\\<\\(" font-lock-hexnumber "\\)\\>" ) 0 font-lock-number-face))))


;;*
;;** font customisation functions
(defun aqua/set-font-size (size)
  "SIZE of the font to be set."
  (interactive "nSize: ")
  (aqua/set-mac-font "DejaVu Sans Mono" size))

(defun aqua/resize-13 (&optional nosplit)
  (interactive "P")
  (aqua/set-font-size 13)
  (aqua/arrange-frame 164 48 nosplit))

(defun aqua/resize-1-col (&optional nosplit)
  (interactive "P")
  (rwd-set-font-size 13)
  (rwd-arrange-frame 80 48 t))

(defun aqua/resize-presentation ()
  "Create a giant font window suitable for doing live demos."
  (interactive)
  (rwd-arrange-frame 92 34 t)
  (rwd-set-font-size 20))

(defun aqua/set-mac-font (name size)
  "NAME and SIZE of the font to be set."
  (interactive
   (list (completing-read "font-name: "
                          (mapcar (lambda (p) (list p p))
                                  (font-family-list)) nil t)
         (read-number "size: " 12)))
  (set-face-attribute 'default nil
                      :family name
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height (* 10 size))
  (frame-parameter nil 'font))

;; (if window-system
;;     (add-hook 'after-init-hook
;;               (lambda () (run-with-idle-timer 0.25 nil #'aqua/resize-13)
;;                          (server-start)) t))

(when (boundp 'aquamacs-version)
  (aquamacs-autoface-mode 0)
  (setq mac-allow-anti-aliasing t)                           ;; anti-aliasing
  (setq ns-use-srgb-colorspace nil)
  (set-face-bold 'bold nil)                                  ;; disable bold fonts
  ;; font settings
  (aqua/set-mac-font "Monaco" 14))



;; *
;;** change the starup message in the echo area
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
