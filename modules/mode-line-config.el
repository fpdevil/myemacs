;;; package --- mode-line-config.el configuration settings for mode line
;; -*- mode: emacs-lisp -*-
;;;
;;; Commentary:
;;;
;;; Filename   : mode-line-config.el
;;; Description: Emacs Modeline themes with SML and Airline
;;;
;;; elisp code for customizing the Emacs mode line
;;;
;;; Code:
;;;
;;===============================================================================

(require-package 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

(require-package 'highlight-quoted)
(add-hook 'prog-mode-hook 'highlight-quoted-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fancy modeline(s) - powerline, airline and sml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'powerline)
(powerline-default-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define constants for holding the themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom aqua-airline-theme nil
  "Select an appropriate Airline Theme."
  :type 'symbol)

(defcustom aqua-sml-theme nil
  "Theme for smart-mode-line."
  :type 'symbol)

;; change the airline themes as required from the below
(setq aqua-airline-theme 'airline-cool)


;; set smart-mode-line theme
(setq aqua-sml-theme 'powerline)
;; alternative themes available:
;; (sml/apply-theme 'powerline)
;; (sml/apply-theme 'dark)
;; (sml/apply-theme 'light)
;; (sml/apply-theme 'respectful)
;; (sml/apply-theme 'automatic)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vim airline theme for emacs modeline customized display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-airline-settings ()
  "Load required Airline theme settings."
  (interactive)
  (require 'airline-themes)
  ;; do not display the current directory
  (setq airline-display-directory nil)
  ;; setting powerline fonts for font glyphs
  (setq powerline-utf-8-separator-left        #xe0b0
        powerline-utf-8-separator-right       #xe0b2
        airline-utf-glyph-separator-left      #xe0b0
        airline-utf-glyph-separator-right     #xe0b2
        airline-utf-glyph-subseparator-left   #xe0b1
        airline-utf-glyph-subseparator-right  #xe0b3
        airline-utf-glyph-branch              #xe0a0
        airline-utf-glyph-readonly            #xe0a2
        airline-utf-glyph-linenumber          #xe0a1))

(defun apply-airline ()
  "Apply the Airline Themes as needed."
  (interactive)
  (load-airline-settings)
  (load-theme aqua-airline-theme t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smart-mode-line enable/disable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun apply-sml ()
  "Apply Smart Mode Line if needed."
  (interactive)
  (require 'smart-mode-line)
  (if (require 'smart-mode-line nil 'noerror)
      (progn
        ;;(setq powerline-arrow-shape 'curve)
        ;;(setq powerline-default-separator-dir '(right . left))
        (setq sml/name-width 20)
        (setq sml/mode-width 'full)
        (setq sml/shorten-directory t)
        (setq sml/shorten-modes t)
        (setq sml/no-confirm-load-theme t)
        (rich-minority-mode 1)
        (if after-init-time
            (sml/setup)
          (add-hook 'after-init-hook #'sml/setup))
        (if (not (null aqua-sml-theme))
            (setq sml/theme aqua-sml-theme)
          ;; delegate theme to the current active one
          (setq sml/theme nil))
        (setq sml/replacer-regexp-list
              '(("^~/org/" ":O:")
                ("^~/code/" ":CODE:")
                ("^~/\\.emacs\\.d/" ":ED:"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uncomment if airline themes are required (load airline settings)
;; (add-hook 'after-init-hook 'load-airline-settings)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-hook 'after-init-hook 'apply-airline)
(when (eq dotemacs-mode-line 'airline)
 (add-hook 'after-init-hook 'apply-airline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uncomment if smart mode line is required
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-hook 'after-init-hook 'apply-sml)
(when (eq dotemacs-mode-line 'sml)
 (add-hook 'after-init-hook 'apply-sml))

;;;;;;;;;;;;;;;;;;;;;;;;; mode line configuration end ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mode-line-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; mode-line-config.el ends here
