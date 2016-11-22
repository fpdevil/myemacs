;;; package --- initialization section for Emacs
;;;
;;;    ___ _ __ ___   __ _  ___ ___
;;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;;  |  __/ | | | | | (_| | (__\__ \
;;;   \___|_| |_| |_|\__,_|\___|___/
;;;
;;; Commentary:
;;;
;;; filename   : init.el
;;; description: initialize the necessary packages
;;;
;; This sets up the load path so that we can override it
;; reference(s)
;; https://hristos.triantafillou.us/init.el/
;; Updated    : 20 Nov 2016
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
; utf-8 encoding
;;
(set-language-environment   'utf-8)
(setq locale-coding-system  'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(prefer-coding-system       'utf-8)

;;
; system is mac os x
;;
(defconst *is-a-mac* (eq system-type 'darwin))


;;
; adding the required lisp files and libraries to path
;;
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d"))
(setq custom-file (concat (getenv "HOME") "/.emacs.d/custom-settings.el"))
(load custom-file)
(load (concat (getenv "HOME") "/.emacs.d/my-package-repos.el"))



;;--------------------------------------------------------------------------;;
;; byte recompiling everything during bootstrap                             ;;
;;--------------------------------------------------------------------------;;
;; uncomment below section if needed
; (byte-recompile-directory (expand-file-name "~/.emacs.d/packages/elpa") 0)
;
; a custom function is defined inside my-methods
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
;; fancy modeline                                                           ;;
;; powerline                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'powerline)
(powerline-default-theme)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          setting default color theme to the required one                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(add-hook 'after-init-hook
      (lambda ()
        ; (load-theme 'cyberpunk t)                     ;; cyberpunk theme
        ; (load-theme 'material t)                      ;; material dark theme
        ; (load-theme 'material-light t)                ;; material light theme
        ; (load-theme 'dracula t)                       ;; dracula dark theme
        (load-theme 'mccarthy)                          ;; from sublime-themes
        ;;
        ;; below for moe-theme ;;
        ; (require 'moe-theme)
        ; ; show highlighted buffer-id as decoration
        ; (setq moe-theme-highlight-buffer-id t)
        ; ; choose a color for mode-line
        ; (moe-theme-set-color 'cyan)
        ; (moe-light)                                     ;; moe-light or moe-dark
        ;;
        )
      )
;(require 'color-theme-sanityinc-solarized)             ;; solarized thene
;(color-theme-sanityinc-solarized-light)                ;; solarized light
;; wait until startup initialization is complete


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; identify unnecessary whitespace is in all programming modes              ;;
;; whitespace-cleanup command for clearing trailing whitespaces             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'whitespace)
(setq-default show-trailing-whitespace t)
(set-default 'indent-tabs-mode nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diminished modes are minor modes with no modeline display                ;;
;; hide a minor mode that you know are always enabled using this            ;;
;; http://www.eskimo.com/~seldon/diminish.el                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'diminish)
(eval-after-load "whitespace" '(diminish 'whitespace-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown mode                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'markdown-mode)
(setq auto-mode-alist
              (cons '("\\.mdml$" . markdown-mode) auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit (git integration)                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'magit)
(eval-after-load 'magit
  (progn '(global-set-key (kbd "C-x g") 'magit-status)))
;(define-key global-map (kbd "C-c m") 'magit-status)


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
;; fancy stuff                                                              ;;
;; rainbow mode                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smart parenthesis matching                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'smartparens)
(smartparens-global-mode t)
(show-smartparens-global-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vim airline theme for emacs modeline customized display                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'airline-themes)
;(load-theme 'airline-light)     ; load airline light theme
(load-theme 'airline-papercolor) ; load papercolor theme
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake-easy (helpers for easily building Emacs flymake checkers)        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake-easy)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show flymake errors in minibuffer                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake-cursor)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippets configuration                                                 ;;
;; this will install and activate it everywhere.                            ;;
;; your snippets are stored in ~/.emacs.d/snippets.                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'yasnippet)
(yas-global-mode 1)
(yas-load-directory (concat (getenv "HOME") "/.emacs.d/snippets"))
(add-hook 'term-mode-hook (lambda()
    (setq yas-dont-activate t)))
(add-to-list 'yas-snippet-dirs (concat (getenv "HOME") "/.emacs.d/snippets/"))
(add-to-list 'yas-snippet-dirs (concat (getenv "HOME") "/.emacs.d/snippets/yasnippet-snippets"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-completion (where company is not available)                         ;;
;; setting up autocomplete after yasnippet to avoid duplciate tab bindings  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete)
(when (require 'auto-complete-config nil 'noerror)
  (add-to-list 'ac-dictionary-directories
    (concat (getenv "HOME") "/.emacs.d/vendor/auto-complete/dict"))
  (setq ac-comphist-file
    (concat (getenv "HOME") "/.emacs.d/ac-comphist.dat"))
  (ac-config-default)
  )
(setq ac-delay 0.0)
(setq ac-quick-help-delay 0.5)
; to enable auto-complete globally
; (global-auto-complete-mode t)
; (setq ac-sources '(ac-source-yasnippet
;                    ac-source-abbrev
;                    ac-source-words-in-same-mode-buffers))
;; show the menu
(setq ac-show-menu-immediately-on-auto-complete t)

;; for disabling auto-complete mode for a mode
; (defadvice auto-complete-mode (around disable-auto-complete-for-progname)
;   (unless (eq major-mode 'progname-mode) ad-do-it))
; (ad-activate 'auto-complete-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org bullets for markdown                                                 ;;
;; use org-bullets-mode for utf8 symbols as org bullets                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org-bullets)
(setq org-bullets-bullet-list
        '("◉" "◎" "⚫" "○" "►" "◇"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
(sequence "⚑ WAITING(w)" "|")
(sequence "|" "✘ CANCELED(c)")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer mode                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'buffer-move)
(global-set-key (kbd "<S-s-up>")     'buf-move-up)
(global-set-key (kbd "<S-s-down>")   'buf-move-down)
(global-set-key (kbd "<S-s-left>")   'buf-move-left)
(global-set-key (kbd "<S-s-right>")  'buf-move-right)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ecb (emacs code browser)                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ecb)
(setq ecb-auto-activate nil)
(setq ecb-layout-name "left13")
(setq ecb-new-ecb-frame nil)
(setq ecb-tip-of-the-day nil)
(setq ecb-windows-width 13)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENhanced Scala Interaction Mode for Emacs (for scala development)        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map pairs of simultaneously pressed keys to commands                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'key-chord)
(key-chord-define-global "fm" 'list-buffers)
(key-chord-define-global "fm" 'helm-mini)
;(use fm instead of C-x b to list buffers)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple cursors for Emacs                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; edit multiple regions simultaneously in a buffer or a region             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'iedit)
(setq iedit-unmatched-lines-invisible-default t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; an autonomous emacs major-mode for editing web templates                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-enable-css-colorization t)
(setq web-mode-code-indent-offset 2)
(setq web-mode-markup-indent-offset 2)


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
