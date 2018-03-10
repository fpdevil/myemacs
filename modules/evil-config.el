;;; package --- evil-config.el configuration settings for vim emulation
;;;
;;; Commentary:
;;;
;;; Filename   : evil-config.el
;;; Description: Emacs Vim Emulation Mode or evil
;;;              https://github.com/noctuid/evil-guide
;;;
;;; elisp code for customizing the evil
;;; undo-tree: undo (C-/) behaves just like normal editor.  To redo, C-_
;;; To open the undo tree, C-x u
;;;
;;; Code:
;;===========================================================================

;;; -- custom conditions
(defgroup dotemacs-evil-modes nil
  "Specify the configuration options for evil-mode."
  :group 'dotemacs
  :prefix 'dotemacs-evil-modes)

(defcustom dotemacs-evil-modes/emacs-state-hooks
  '(org-log-buffer-setup-hook org-capture-mode-hook)
  "List of all hooks to automatically start up in Evil Emacs state."
  :type '(repeat (symbol))
  :group 'dotemacs-evil-modes)

(defcustom dotemacs-evil-modes/emacs-state-major-modes
  '(eshell-mode
    term-mode
    calculator-mode
    makey-key-mode)
  "List of all major modes that should default to Emacs state."
  :type '(repeat (symbol))
  :group 'dotemacs-evil-modes)

(defcustom dotemacs-evil-modes/emacs-state-minor-modes
  '(edebug-mode
    git-commit-mode
    magit-blame-mode)
  "List of all minor modes that when active should switch to Emacs state."
  :type '(repeat (symbol))
  :group 'dotemacs-evil-modes)

(defcustom dotemacs-evil-modes/emacs-insert-mode
  nil
  "If this options is non-nil, insert mode will act as Emacs state."
  :type 'boolean
  :group 'dotemacs-evil-modes)

;;; -- evil search
(setq evil-search-module 'evil-search)
(setq evil-magic 'very-magic)

;;; change cursor colors based on the mode
(setq evil-emacs-state-cursor    '("#cc6666" box))
(setq evil-motion-state-cursor   '("orange"  box))
(setq evil-normal-state-cursor   '("#b294bb" box))
(setq evil-visual-state-cursor   '("orange"  box))
(setq evil-insert-state-cursor   '("#de935f" bar))
(setq evil-replace-state-cursor  '("red"     bar))
(setq evil-operator-state-cursor '("red"     hollow))

(add-hook 'evil-jumps-post-jump-hook #'recenter)

;;; -- evil mode (for vim emulation)
(require-package 'evil)
(require 'evil)
(evil-mode)                                   ;; enable evil-mode globally

;;; == define conditional loop to graze trough all the available
;;;    minor modes and add them to evil
(cl-loop for mode in dotemacs-evil-modes/emacs-state-minor-modes
         do (let ((hook (concat (symbol-name mode) "-hook")))
              (add-hook (intern hook) `(lambda ()
                                         (if ,mode
                                             (evil-emacs-state)
                                           (evil-normal-state))))))

;;; == define conditional loop to graze trough all the hooks which
;;;    need to start automaticaly in evil mode
(cl-loop for hook in dotemacs-evil-modes/emacs-state-hooks
         do (add-hook hook #'evil-emacs-state))

;;; == define conditional loop to graze through all the available
;;;    major modes to default to Emacs state
(cl-loop for mode in dotemacs-evil-modes/emacs-state-major-modes
         do (evil-set-initial-state mode 'emacs))

;;; -- activate evil-mc and evil-smartparens
;; (evil-mc-mode  1)                ;; enable
;; (evil-mc-mode -1)                ;; disable
(global-evil-mc-mode  1)            ;; enable
;; (global-evil-mc-mode -1)         ;; disable
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

;;; -- evil operations for various vim states
(after 'evil-common
  (evil-put-property 'evil-state-properties 'normal   :tag " NORMAL ")
  (evil-put-property 'evil-state-properties 'insert   :tag " INSERT ")
  (evil-put-property 'evil-state-properties 'visual   :tag " VISUAL ")
  (evil-put-property 'evil-state-properties 'motion   :tag " MOTION ")
  (evil-put-property 'evil-state-properties 'emacs    :tag " EMACS ")
  (evil-put-property 'evil-state-properties 'replace  :tag " REPLACE ")
  (evil-put-property 'evil-state-properties 'operator :tag " OPERATOR "))

;;; == escape from the evil mode
(when dotemacs-evil-modes/emacs-insert-mode
  (defalias 'evil-insert-state 'evil-emacs-state)
  (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state)
  ;;(define-key evil-emacs-state-map (kbd "C-SPC") 'auto-complete)
  )

(unless (display-graphic-p)
  (evil-esc-mode))

;;; -- commenting the blocks with evil
(require-package 'evil-commentary)
(evil-commentary-mode t)

;;; -- vim surround emulation for Emacs
(require-package 'evil-surround)
(global-evil-surround-mode t)
;; use `s' for surround instead of `substitute'
(evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
(evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)

;;; -- Port of vim-exchange
(require-package 'evil-exchange)
(evil-exchange-install)

;;; -- anzu for Evil
(require-package 'evil-anzu)
(require 'evil-anzu)

;;; -- Make ediff a little more evil
(require-package 'evil-ediff)
(evil-ediff-init)

;;; -- magit for evil
(after 'magit
  (require-package 'evil-magit)
  (require 'evil-magit))

;;; -- Evil motion with avy: choose where after which
(require-package 'evil-avy)
(evil-avy-mode)

;;; Vim matchit ported into Emacs
(require-package 'evil-matchit)
(defun evilmi-customize-keybinding ()
  (evil-define-key 'normal evil-matchit-mode-map
    "%" 'evilmi-jump-items))
(global-evil-matchit-mode t)

;;; -- Textobject for evil based on indentation
(require-package 'evil-indent-textobject)
(require 'evil-indent-textobject)

;;; -- Start a * or # search from the visual selection (similar to vim)
(require-package 'evil-visualstar)
(global-evil-visualstar-mode t)

;;; -- Increment and decrement numbers in Emacs
(require-package 'evil-numbers)

;;; -- evil terminal integration
(defun my-send-string-to-terminal (string)
  (unless (display-graphic-p) (send-string-to-terminal string)))

(defun my-evil-terminal-cursor-change ()
  (when (string= (getenv "TERM_PROGRAM") "iTerm.app")
    (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\e]50;CursorShape=1\x7")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\e]50;CursorShape=0\x7"))))
  (when (and (getenv "TMUX") (string= (getenv "TERM_PROGRAM") "iTerm.app"))
    (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=1\x7\e\\")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=0\x7\e\\")))))

(add-hook 'after-make-frame-functions (lambda (frame) (my-evil-terminal-cursor-change)))
(my-evil-terminal-cursor-change)

(defadvice evil-ex-search-next (after dotemacs activate)
  (recenter))

(defadvice evil-ex-search-previous (after dotemacs activate)
  (recenter))

;;; -- Leader Key itegration aka VIM
;;; -- default leader key is - enable evil-leader globally
(require-package 'evil-leader)
(require 'evil-leader)
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader "-")
(evil-leader/set-key
  ;;"e" 'find-file
  "bb" 'switch-to-buffer
  "k" 'kill-buffer
  "bd" 'kill-buffer-and-window
  "by" 'copy-whole-buffer
  "cy" 'clipboard-kill-ring-save
  "cp" 'clipboard-yank
  "bs" 'save-buffer
  "gs" 'magit-status
  "hs" 'split-window-horizontally
  "lf" 'load-file
  "ne" 'flycheck-next-error
  "pe" 'flycheck-previous-error
  "si" 'whitespace-mode
  "tn" 'linum-mode
  "w1" 'delete-other-windows
  "wk" 'windmove-left
  "wj" 'windmove-right
  "qs" 'save-buffers-kill-emacs
  "il" 'imenu-list-minor-mode
  "dn" 'dired-hacks-next-file
  "dp" 'dired-hacks-previous-file
  "dm" 'dired-filter-map)

;;; -- bind ':ls' command to 'ibuffer instead of 'list-buffers
(evil-ex-define-cmd "ls" 'ibuffer)
;;; -- show ibuffer in the same window
(setq ibuffer-use-other-window t)

;;; -- enable search in the vim style
(add-hook 'occur-mode-hook
          (lambda ()
            (evil-add-hjkl-bindings occur-mode-map 'emacs
              (kbd "/") 'evil-search-forward
              (kbd "n") 'evil-search-next
              (kbd "N") 'evil-search-previous)))

;;; --
;;; -- evil mode states (comment / un-comment)
;; (setq evil-default-state 'normal)              ;; if default state is to set normal
;; (setq evil-default-state 'insert)              ;; if default state is to be set emacs
;; (setq evil-default-state 'emacs)               ;; if default state is to be set emacs

;;; == clear evil's white-lists of modes that should start in a particular state,
;;;    so they all start in Emacs state
(setq-default evil-insert-state-modes '())

;;; -- prevent esc-key from translating to meta-key in terminal mode
(setq evil-esc-delay 0)

(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map
  (read-kbd-macro evil-toggle-key) 'evil-emacs-state)

;; make sure ESC key in insert-state will call evil-normal-state
(define-key evil-insert-state-map [escape] 'evil-normal-state)

;; make all emacs-state buffer become to insert-state
(dolist (m evil-emacs-state-modes)
  (add-to-list 'evil-insert-state-modes m))


;;; -- function for toggling the emacs evil states using M-u
(defun toggle-evilmode ()
  "Toggle the evil mode states."
  (interactive)
  (if (bound-and-true-p evil-local-mode)
      (progn
        ;; go emacs
        (evil-local-mode (or -1 1))
        (undo-tree-mode (or -1 1))
        (set-variable 'cursor-type 'bar))
    (progn
      ;; go evil
      (evil-local-mode (or 1 1))
      (set-variable 'cursor-type 'box))))

(global-set-key (kbd "M-u") 'toggle-evilmode)

;;; -- TAB Mode with C-i in evil-mode
(when evil-want-C-i-jump
  (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward))

;;; -- evil plugins
;;; -- evil extension to integrate nicely with paredit
; (require-package 'evil-paredit)
; (require 'evil-paredit)
; (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)

;; -- get rid of the emc in the mode line when multiple cursors are not used
(require-package 'evil-mc)
(require 'evil-mc)                 ;; evil multiple cursors
(setq evil-mc-mode-line
  '(:eval (when (> (evil-mc-get-cursor-count) 1)
            (format ,(propertize " %s:%d " 'face 'cursor)
                    evil-mc-mode-line-prefix
                    (evil-mc-get-cursor-count)))))

;; company evil hooks
(after 'company-mode
  (if (fboundp 'evil-declare-change-repeat)
      (mapc #'evil-declare-change-repeat
            '(company-complete-common
              company-select-next
              company-select-previous
              company-complete-selection
              company-complete-number))))

;; suppress the ad-handle definition warning messages
(setq ad-redefinition-action 'accept)

(provide 'evil-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; evil-config.el ends here
