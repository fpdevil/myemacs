;;; package --- initialization section for Emacs
;;;
;;; name: init.el
;;; description: initialize the packages
;;;
;;; Commentary:
;;
;; This sets up the load path so that we can override it
;; reference
;; https://hristos.triantafillou.us/init.el/
;; http://y.tsutsumi.io/emacs-from-scratch-part-2-package-management.html

;;----------------------------------------------------------------------
;;
; utf-8 encoding
;;
(set-language-environment   'utf-8)
(setq locale-coding-system  'utf-8)
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))


;(package-initialize)

;; system is mac
(defconst *is-a-mac* (eq system-type 'darwin))


(add-to-list 'load-path "~/.emacs.d")
(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file)
(load "~/.emacs.d/my-package-repos.el")


;;
;-----------------------------------------------------------------------
; byte recompiling everything during bootstrap
;-----------------------------------------------------------------------
;;
;; uncomment below section if needed
;(byte-recompile-directory (expand-file-name "~/.emacs.d/packages/elpa") 0)
;
; custom function defined inside my-methods



;; Finalizers
;; for debugging
(setq debug-on-error t)
; (setq debug-on-error nil)
; (setq debug-on-quit nil)

;-----------------------------------------------------------------------


;;======================================================================
;;                            require packages
;;======================================================================

;;
; company mode
;;
(require 'company)
(require 'company-distel)
(add-hook 'after-init-hook 'global-company-mode)
(auto-complete-mode 1)
; no delay for company suggestions
(setq company-idle-delay 0)


;;
; Diminished modes are minor modes with no modeline display
; http://www.eskimo.com/~seldon/diminish.el
;;
(require 'diminish)
(eval-after-load "whitespace" '(diminish 'whitespace-mode))


;;
; markdown mode
;;
(require 'markdown-mode)
(setq auto-mode-alist
              (cons '("\\.mdml$" . markdown-mode) auto-mode-alist))


;;
; magit (gut integration)
;;
(require 'magit)
(define-key global-map (kbd "C-c m") 'magit-status)


;;
; fancy stuff
; for rainbow delimiters
;;
(require 'rainbow-delimiters)


;;
; fancy stuff
; for rainbow identifiers
;;
(require 'rainbow-identifiers)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
;; rainbow identifier customizations
;; Use a wider set of colors
(setq rainbow-identifiers-choose-face-function
      'rainbow-identifiers-cie-l*a*b*-choose-face)
(setq rainbow-identifiers-cie-l*a*b*-lightness 45)
(setq rainbow-identifiers-cie-l*a*b*-saturation 45)



;;
; fancy stuff
; rainbow mode
;;
(require 'rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)


;;
; auto-completion
;;
(require 'auto-complete)
(require 'auto-complete-config)


;;
; smart parenthesis matching
;;
(require 'smartparens)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
; (show-smartparens-global-mode +1)
; (smartparens-global-mode 1)


;;
; syntax checking for GNU Emacs - http://www.flycheck.org/
;;
(require 'flycheck)
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically
  '(mode-enabled save new-line idle-change))
;(add-hook 'prog-mode-hook (lambda () (flycheck-mode)))
;; Improved Haskell support for Flycheck
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))



;;
; flymake-easy (helpers for easily building Emacs flymake checkers)
;;
(require 'flymake-easy)


;;
; show flymake errors in minibuffer
;;
(require 'flymake-cursor)


;;
; flymake handler for checking Haskell source code with hlint.
;;
(require 'flymake-hlint) ;; not needed if installed via package
(add-hook 'haskell-mode-hook 'flymake-hlint-load)


;;
; flymake handler for syntax-checking Python source code using pyflakes or flake8
;;
(require 'flymake-python-pyflakes)
;; (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
;; using flake8
(setq flymake-python-pyflakes-executable "/usr/local/bin/flake8")


;;
; fancy modeline
; powerline
;;
(require 'powerline)
(powerline-default-theme)


;;
; utility package to collect various Icon Fonts and propertize them within Emacs
;;
(require 'all-the-icons)


;;
; automatic and manual symbol highlighting for Emacs
; load from highlight-symbol-config.el
;;
;(require 'highlight-symbol)
; (global-set-key [(control f3)] 'highlight-symbol)
; (global-set-key [f3] 'highlight-symbol-next)
; (global-set-key [(shift f3)] 'highlight-symbol-prev)
; (global-set-key [(meta f3)] 'highlight-symbol-query-replace)



;;
; vim airline theme for emacs
;;
(require 'airline-themes)
;(load-theme 'airline-light)
(load-theme 'airline-papercolor)
; setting powerline fonts
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
; setting default theme tp material dark
;;
; (load-theme 'material t)
(load-theme 'material-light t)
;(load-theme 'darkokai t)


;;
; yasnippets configuration
; this will install and activate it everywhere.
; your snippets are stored in ~/.emacs.d/snippets.
;;
(require 'yasnippet)
(yas-global-mode 1)
(yas-load-directory "~/.emacs.d/snippets")
(add-hook 'term-mode-hook (lambda()
    (setq yas-dont-activate t)))
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/")
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/yasnippet-snippets")
; (yas/load-directory "~/.emacs.d/snippets"))


;;
; auto-complete configuration
;;
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete/dict")
(ac-config-default)
(global-auto-complete-mode t)
; (setq ac-sources '(ac-source-yasnippet
;                    ac-source-abbrev
;                    ac-source-words-in-same-mode-buffers))
;; show the menu
(setq ac-show-menu-immediately-on-auto-complete t)


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;                  python support specific
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; defined inside python-config.el
;


;;
; org bullets for markdown
; use org-bullets-mode for utf8 symbols as org bullets
;;
(require 'org-bullets)
(setq org-bullets-bullet-list
        '("◉" "◎" "⚫" "○" "►" "◇"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
(sequence "⚑ WAITING(w)" "|")
(sequence "|" "✘ CANCELED(c)")))



;;
; buffer mode
;;
(require 'buffer-move)
(global-set-key (kbd "<S-s-up>")     'buf-move-up)
(global-set-key (kbd "<S-s-down>")   'buf-move-down)
(global-set-key (kbd "<S-s-left>")   'buf-move-left)
(global-set-key (kbd "<S-s-right>")  'buf-move-right)

;;
; ecb (emacs code browser)
;;
(require 'ecb)
(setq ecb-auto-activate nil)
(setq ecb-layout-name "left13")
(setq ecb-new-ecb-frame nil)
(setq ecb-tip-of-the-day nil)
(setq ecb-windows-width 13)


;;
; ENhanced Scala Interaction Mode for Emacs
;;
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


;;
; haskell setup
; full configuration inside haskell-config.el
;;
(require 'haskell-mode)
(require 'hindent)


;;
; erlang setup
; full configuration inside erlang-config.el
;;
(require 'erlang-start)


;;
; Map pairs of simultaneously pressed keys to commands
;;
(require 'key-chord)
(key-chord-define-global "fm" 'list-buffers)
;(use fm instead of C-x b to list buffers)



;;
; helm
; Emacs incremental completion and selection narrowing framework
;;
(require 'helm-config)
(helm-mode 1)
; listing files
(global-set-key (kbd "C-x C-f") 'helm-find-files)
; listing opened buffers
(global-set-key (kbd "C-x b") 'helm-mini)
(key-chord-define-global "fm" 'helm-mini)



;;
; show flycheck/flymake errors by tooltip
;;
(require 'flycheck-tip)



;;
; multiple cursors for Emacs
;;
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ido disabled in favour of helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; intero disabled in favour of other haskell packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
; Complete interactive development environment for Haskell
;;
;(require 'intero)
;;



(provide 'init)

;;; init.el ends here
