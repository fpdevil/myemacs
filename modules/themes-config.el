;;; package --- themes-config.el configuration settings for color theme
;; -*- mode: emacs-lisp -*-
;;;
;;; Commentary:
;;;
;;; Filename   : themes-config.el
;;; Description: Emacs Color theme
;;;
;;; elisp code for customizing the color theme (from Steve Purcell)
;;===========================================================================
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fancy modeline                                                         ;;;
;;; powerline                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'powerline)
(powerline-default-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vim airline theme for emacs modeline customized display                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'airline-themes)
; setting powerline fonts for glyphs
(setq powerline-utf-8-separator-left        #xe0b0
      powerline-utf-8-separator-right       #xe0b2
      airline-utf-glyph-separator-left      #xe0b0
      airline-utf-glyph-separator-right     #xe0b2
      airline-utf-glyph-subseparator-left   #xe0b1
      airline-utf-glyph-subseparator-right  #xe0b3
      airline-utf-glyph-branch              #xe0a0
      airline-utf-glyph-readonly            #xe0a2
      airline-utf-glyph-linenumber          #xe0a1)

;;
; change the airline themes as required from the below
;;
;(load-theme 'airline-light t)                ; light theme
;(load-theme 'airline-papercolor t)           ; papercolor theme
;(load-theme 'airline-badwolf t)              ; badwwolf theme
(load-theme 'airline-cool t)                 ; cool theme

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setting default color theme to required (from Steve Purcell .emacs)    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default custom-enabled-themes '(;sanityinc-solarized-light
                                      ;sanityinc-tomorrow-day
                                      ;material
                                      ;material-light
                                      ;mccarthy
                                      ;flatui
                                      ;majapahit-light
                                      ))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

;;------------------------------------------------------------------------------
;; Toggle between light and dark solarized themes
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (require 'color-theme-sanityinc-solarized)
  (color-theme-sanityinc-solarized-light))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (require 'color-theme-sanityinc-tomorrow)
  (color-theme-sanityinc-solarized-dark))


(provide 'themes-config)
;;; themes-config.el ends here
