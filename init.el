;;; package --- Aquamacs initialization file
;;;
;;;    ___ _ __ ___   __ _  ___ ___
;;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;;  |  __/ | | | | | (_| | (__\__ \
;;;   \___|_| |_| |_|\__,_|\___|___/
;;;
;;; Author    : Sampath Singamsetty <Singansetty.Sampath@gmail.com>
;;; URL       : https://github.com/singamsetty/myemacs
;;; Commentary:
;;;
;;; filename   : init.el
;;; description: initialization file for loading the necessary packages
;;;
;; This sets up the load path so that we can override it
;; Updated    : 01 Dec 2016
;;;===========================================================================
;;;
;;; Code:
;;;

;;
; utf-8 character set encoding
;;
(set-language-environment   'utf-8)
(setq locale-coding-system  'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(prefer-coding-system       'utf-8)

;;
; if system is mac os x
;;
(defconst *is-a-mac* (eq system-type 'darwin))


;;
; adding the required lisp files and libraries to path
; custom-settings.el will store any custom settings made on Emacs
;;
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d"))
(setq custom-file (concat (getenv "HOME") "/.emacs.d/custom-settings.el"))
(load custom-file)
(load (concat (getenv "HOME") "/.emacs.d/aqua-package-repos.el"))

;;--------------------------------------------------------------------------;;
;; byte recompiling everything during bootstrap                             ;;
;; a custom function is defined inside my-methods                           ;;
;; uncomment below section if needed                                        ;;
;;--------------------------------------------------------------------------;;
; (byte-recompile-directory
;   (expand-file-name (concat (getenv "HOME") "/.emacs.d/packages/elpa/")) 0)

;;--------------------------------------------------------------------------;;
;;
; Finalizers
; for debugging
;;
(setq debug-on-error t)
; (setq debug-on-error nil)
; (setq debug-on-quit nil)

;;==========================================================================;;
;;            specify all the require packages and settings                 ;;
;;==========================================================================;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diminished modes are minor modes with no modeline display                ;;
;; hide a minor mode that you know are always enabled using this            ;;
;; http://www.eskimo.com/~seldon/diminish.el                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'diminish)
(eval-after-load "whitespace" '(diminish 'whitespace-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fancy modeline                                                           ;;
;; powerline                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(require 'powerline)
;(powerline-default-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          setting default color theme to the required one                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(add-hook 'after-init-hook
      (lambda ()
        ; (load-theme 'sanityinc-solarized-light)         ;; solarized light theme
        ; (load-theme 'cyberpunk t)                       ;; cyberpunk theme
        ; (load-theme 'material t)                        ;; material dark theme
        (load-theme 'material-light t)                  ;; material light theme
        ; (load-theme 'dracula t)                         ;; dracula dark theme
        ; (load-theme 'mccarthy)                          ;; mccarthy from sublime-themes
        ; (load-theme 'apropospriate-dark t)              ;; apropospriate dark theme
        ; (load-theme 'apropospriate-light t)             ;; apropospriate light theme
        ;;
        ;; -- below for activating moe-theme -- ;;
        ; (require 'moe-theme)
        ; ; show highlighted buffer-id as decoration
        ; (setq moe-theme-highlight-buffer-id t)
        ; ; choose a color for mode-line
        ; (moe-theme-set-color 'cyan)
        ; (moe-light)                                     ;; moe-light or moe-dark
        ;;
        )
      )
;; wait until startup initialization is complete

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vim airline theme for emacs modeline customized display                  ;;
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
;(load-theme 'airline-light)                ; load airline light theme
(load-theme 'airline-papercolor)            ; load papercolor theme
;(load-theme 'airline-base16-shell-dark)    ; load airline-base16-shell-dark theme

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; identify unnecessary whitespace is in all programming modes              ;;
;; whitespace-cleanup command for clearing trailing white spaces            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'whitespace)
(setq-default show-trailing-whitespace t)
(set-default 'indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editing files in markdown mode                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'markdown-mode)
(setq auto-mode-alist
              (cons '("\\.mdml$" . markdown-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit (git integration - now using git-gutter)                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (require 'magit)
; (eval-after-load 'magit
;   (progn '(global-set-key (kbd "C-x g") 'magit-status)))
; (define-key global-map (kbd "C-c m") 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fancy (but useful) stuff                                                 ;;
;; for rainbow delimiters                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rainbow-delimiters)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fancy (but useful) stuff                                                 ;;
;; for rainbow identifiers                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rainbow-identifiers)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
;; rainbow identifier customizations
;; use a wider set of colors
(setq rainbow-identifiers-choose-face-function
      'rainbow-identifiers-cie-l*a*b*-choose-face)
(setq rainbow-identifiers-cie-l*a*b*-lightness 45)
(setq rainbow-identifiers-cie-l*a*b*-saturation 45)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake-easy (helpers for easily building Emacs flymake checkers)        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake-easy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show flymake errors in mini buffer                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake-cursor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer mode                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'buffer-move)
(global-set-key (kbd "<S-s-up>")     'buf-move-up)
(global-set-key (kbd "<S-s-down>")   'buf-move-down)
(global-set-key (kbd "<S-s-left>")   'buf-move-left)
(global-set-key (kbd "<S-s-right>")  'buf-move-right)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map pairs of simultaneously pressed keys to commands                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'key-chord)
(key-chord-define-global "fm" 'list-buffers)
(key-chord-define-global "fm" 'helm-mini)
;(use fm instead of C-x b to list buffers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; edit multiple regions simultaneously in a buffer or a region             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'iedit)
(setq iedit-unmatched-lines-invisible-default t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ido disabled in favour of helm                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ido settings
; Interactively Do Things
; using helm now
;;
; (require 'ido)
; (ido-mode t)
; (setq ido-enable-flex-matching t)
; (setq ido-everywhere t)
; (setq ido-max-prospects 50)
; (setq ido-max-window-height 0.25)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init)

;;; init.el ends here
