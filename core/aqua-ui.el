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

;;* for title bar (OS X emacs 26.1)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

;;* display a marker on the left fringe for lines not in the buffer
(setq indicate-empty-lines t)

;;* show no tool-bar
;; (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;;* show no menu-bar
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;* show no scroll-bar
(if (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))


;;** frame title to show either a file or a buffer name
(setq frame-title-format
      '("" invocation-name " Emacs - " (:eval (if (buffer-file-name)
                                                  (abbreviate-file-name (buffer-file-name))
                                                "%b"))))

;;** show parentheses
(show-paren-mode t)
(setq show-paren-style 'parenthesis)    ;; highlight brackets
;;(setq show-paren-style 'expression)   ;; highlight entire expression
;;(setq show-paren-style 'mixed)        ;; highlight brackets if visible, else entire expression

;;** get a visual indication of an exception
(set 'visible-bell t)

;;** syntax highlighting everywhere
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

;;**  add a proper word wrapping
;;*** enable visual wordwrap in every text-mode (which includes org-mode)
;; how long lines are handled, this appears to wrap long lines visually,
;; but not add line-returns
(global-visual-line-mode 1)

;;** additional mode line settings
(line-number-mode 1)
(column-number-mode t)
(size-indication-mode t)

;;** for pdf viewing
(setq doc-view-continuous t)

;;** display settings
;; no mode-specific faces, everything in Monaco
(when (boundp 'aquamacs-version)
  (aquamacs-autoface-mode -1)
  (set-face-attribute 'mode-line nil :inherit 'unspecified)   ; show modeline in Monaco
  (set-face-attribute 'echo-area nil :family 'unspecified)    ; show echo area in Monaco
  ;; opening Files within Same Frame, but keep Apple-N functionality the same
  (setq one-buffer-one-frame-mode 0))

;;** set a default font (monaco 12) for Aquamacs
(when (boundp 'aquamacs-version)
  (setq aquamacs-additional-fontsets t))

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

;;** font customisation functions
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
                      :weight 'light
                      :width  'normal
                      :height (* 10 size))
  (frame-parameter nil 'font))

;;** set font and settings if for Aquamacs
(when (boundp 'aquamacs-version)
  (aquamacs-autoface-mode 0)
  (setq mac-allow-anti-aliasing t)      ; anti-aliasing
  (setq ns-use-srgb-colorspace nil)
  (set-face-bold 'bold nil)             ; disable bold fonts
  ;; (aqua/set-mac-font "Source Code Pro for Powerline" 14)
  (aqua/set-mac-font "Monaco for Powerline" 14))

;;** set font if for Emacs
(when (boundp 'emacs-version)
  (setq mac-allow-anti-aliasing t)      ; anti-aliasing
  (setq ns-use-srgb-colorspace nil)
  (set-face-bold 'bold nil)             ; disable bold fonts
  ;; (aqua/set-mac-font "Monaco for Powerline" 14)
  (aqua/set-mac-font "Monaco for Powerline" 14))

;;** for Emoji fontset
(set-fontset-font t 'symbol
                  (font-spec :family "Apple Color Emoji")
                  nil 'prepend)

;;** change the starup message in the echo area
(defun display-startup-echo-area-message ()
  "Startup echo message."
  (message "Let the hacking begin!"))


(provide 'aqua-ui)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; aqua-ui.el ends here
