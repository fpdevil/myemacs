;; This sets up the load path so that we can override it
;; reference
;; https://hristos.triantafillou.us/init.el/
;; http://y.tsutsumi.io/emacs-from-scratch-part-2-package-management.html

;; utf-8 encoding
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)

;(package-initialize)

(defconst *is-a-mac* (eq system-type 'darwin))

(add-to-list 'load-path "~/.emacs.d")
(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file)

(setq user-full-name "Sampath Singamsetty"
      user-mail-address "Singamsetty.Sampath@gmail.com")

(load "~/.emacs.d/my-package-repos.el")

;;
; byte recompiling everything
;;
(byte-recompile-directory (expand-file-name "~/Library/Preferences/Aquamacs Emacs/Packages/elpa") 0)


;;--------------------------------------
;; require packages
;;--------------------------------------

;; Diminished modes are minor modes with no modeline display
;; http://www.eskimo.com/~seldon/diminish.el
(require 'diminish)
(eval-after-load "whitespace" '(diminish 'whitespace-mode))


;;
; company mode
;;
(require 'company)
(global-company-mode t)


;;
; company mode for jedi
;;
(require 'company-jedi)


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
; for rainbow delimiters
;;
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; some customizations for the default modes
'(set-face-attribute rainbow-delimiters-depth-1-face
  ((t (:foreground "dark orange"))))
'(set-face-attribute rainbow-delimiters-depth-2-face
  ((t (:foreground "deep pink"))))
'(set-face-attribute rainbow-delimiters-depth-3-face
  ((t (:foreground "chartreuse"))))
'(set-face-attribute rainbow-delimiters-depth-4-face
  ((t (:foreground "deep sky blue"))))
'(set-face-attribute rainbow-delimiters-depth-5-face
  ((t (:foreground "yellow"))))
'(set-face-attribute rainbow-delimiters-depth-6-face
  ((t (:foreground "orchid"))))
'(set-face-attribute rainbow-delimiters-depth-7-face
  ((t (:foreground "spring green"))))
'(set-face-attribute rainbow-delimiters-depth-8-face
  ((t (:foreground "sienna1"))))


;;
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
; rainbow mode
;;
(require 'rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)

;;
; auto-completion
;;
(require 'auto-complete)

;;
; smart parenthesis matching
;;
(require 'smartparens)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)


;;
; syntax checking for GNU Emacs - http://www.flycheck.org/
;;
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)


;;
; powerline
;;
(require 'powerline)
(powerline-default-theme)


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
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(global-auto-complete-mode t)
; (setq ac-sources '(ac-source-yasnippet
;                    ac-source-abbrev
;                    ac-source-words-in-same-mode-buffers))
;; show the menu
(setq ac-show-menu-immediately-on-auto-complete t)


;;
; python jedi
; python development with auto-completion and intelli-sense
;;
(require 'jedi)

;; Start auto-complete and jedi for refactoring
(setq flymake-log-level 3)
;; Hook up to autocomplete
; (add-to-list ’ac-sources ’ac-source-jedi-direct)
(autoload 'jedi:setup "jedi" nil t)
;; enable python-mode
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi-config:set-python-executable "/usr/local/bin/python3")
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
; hack to never show in-function call automatically
(setq jedi:get-in-function-call-delay 300)
(setq jedi:server-command (list "/usr/local/bin/python3" jedi:server-script))
(jedi-mode 1)
(setq jedi:environment-root "env")
; (setq jedi:environment-virtualenv
;   (append python-environment-virtualenv
;     '(' "--python" "/usr/local/bin/python3")))

(defun jedi-config:setup-keys ()
       (local-set-key (kbd "M-.") 'jedi:goto-definition)
       (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
       (local-set-key (kbd "M-?") 'jedi:show-doc)
       (local-set-key (kbd "M-/") 'jedi:get-in-function-call))
(add-hook 'python-mode-hook 'jedi-config:setup-keys)

(setq py-python-command "/usr/local/bin/python3")
(setq python-shell-interpreter "/usr/local/bin/ipython3")
(setq python-check-command "/usr/local/bin/pyflakes")
(setq python-environment-directory "~/.emacs.d/.python-environments")

; system path in the lisp
; set PATH, because we don't load .bashrc
(setenv
 "PATH" (concat
   "$HOME/bin:"
   "/bin:"
   "/usr/bin:"
   "/sbin:"
   "/usr/sbin:"
   "/usr/local/bin:"
   "/usr/local/sbin"))

; Set PYTHONPATH, because we don't load .bashrc
(setenv "PYTHONPATH" "/usr/local/lib/python3.5/site-packages:")


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
; ido settings
; Interactively Do Things
;;
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-max-prospects 50)
(setq ido-max-window-height 0.25)


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
; scala development
;;
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;
; haskell setup
;;
(require 'haskell-mode)


;;
; Emacs incremental completion and selection narrowing framework
;;
(require 'helm-config)

;;
; Complete interactive development program for Haskell
;;
;(require 'intero)