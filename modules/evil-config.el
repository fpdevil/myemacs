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
;;===========================================================================
(require 'evil)
(require 'undo-tree)
(require 'evil-leader)
(require 'evil-paredit)            ;; extension to integrate nicely with paredit
(require 'evil-mc)                 ;; evil multiple cursors
;(require 'evil-tabs)
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil mode (for vim emulation)                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-mode t)                                   ;; enable evil-mode globally
(global-undo-tree-mode)                         ;; enable undo-tree globally

;;---------------------------------------------------------------------------+
;; activate evil-mc and evil-smartparens                                     |
;;---------------------------------------------------------------------------+
;; (evil-mc-mode  1) ;; enable
;; (evil-mc-mode -1) ;; disable
(global-evil-mc-mode  1) ;; enable
;; (global-evil-mc-mode -1) ;; disable
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

;;---------------------------------------------------------------------------+
;; evil search                                                               |
;;---------------------------------------------------------------------------+
(setq evil-search-module 'evil-search)
(setq evil-magic 'very-magic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; change cursor colors based on the mode                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq evil-emacs-state-cursor    '("red" box))
(setq evil-motion-state-cursor   '("orange" box))
(setq evil-normal-state-cursor   '("green" box))
(setq evil-visual-state-cursor   '("orange" box))
(setq evil-insert-state-cursor   '("red" bar))
(setq evil-replace-state-cursor  '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

(add-hook 'evil-jumps-post-jump-hook #'recenter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil operations for various vim states                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-put-property 'evil-state-properties 'normal   :tag " NORMAL ")
(evil-put-property 'evil-state-properties 'insert   :tag " INSERT ")
(evil-put-property 'evil-state-properties 'visual   :tag " VISUAL ")
(evil-put-property 'evil-state-properties 'motion   :tag " MOTION ")
(evil-put-property 'evil-state-properties 'emacs    :tag " EMACS ")
(evil-put-property 'evil-state-properties 'replace  :tag " REPLACE ")
(evil-put-property 'evil-state-properties 'operator :tag " OPERTR ")

;;
; evil mode states (comment / un-comment)
;;
; (setq evil-default-state 'insert)             ;; if default state is to be set emacs
; (setq evil-default-state 'emacs)              ;; if default state is to be set emacs
(setq evil-default-state 'normal)               ;; if default state is to set normal


;; prevent esc-key from translating to meta-key in terminal mode
(setq evil-esc-delay 0)


(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map
  (read-kbd-macro evil-toggle-key) 'evil-emacs-state)

;; make sure ESC key in insert-state will call evil-normal-state
(define-key evil-insert-state-map [escape] 'evil-normal-state)

;; make all emacs-state buffer become to insert-state
(dolist (m evil-emacs-state-modes)
  (add-to-list 'evil-insert-state-modes m))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default leader key is \ enable evil-leader globally                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader "-")
(evil-leader/set-key
  "e" 'find-file
  "bb" 'switch-to-buffer
  "k" 'kill-buffer
  "bd" 'kill-buffer-and-window
  "by" 'copy-whole-buffer
  "cy" 'clipboard-kill-ring-save
  "cp" 'clipboard-yank
  "fs" 'save-buffer
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
  "qq" 'save-buffers-kill-emacs
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bind ':ls' command to 'ibuffer instead of 'list-buffers                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-ex-define-cmd "ls" 'ibuffer)
;; show ibuffer in the same window
(setq ibuffer-use-other-window t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; enable search in the vim style                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'occur-mode-hook
          (lambda ()
            (evil-add-hjkl-bindings occur-mode-map 'emacs
              (kbd "/") 'evil-search-forward
              (kbd "n") 'evil-search-next
              (kbd "N") 'evil-search-previous)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; function for toggling the emacs evil states using M-u                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun toggle-evilmode ()
  "Toggle the evil mode states."
  (interactive)
  (if (bound-and-true-p evil-local-mode)
    (progn
      ; go emacs
      (evil-local-mode (or -1 1))
      (undo-tree-mode (or -1 1))
      (set-variable 'cursor-type 'bar)
    )
    (progn
      ; go evil
      (evil-local-mode (or 1 1))
      (set-variable 'cursor-type 'box)
    )
  )
)

(global-set-key (kbd "M-u") 'toggle-evilmode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TAB Mode with C-i in evil-mode                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when evil-want-C-i-jump
  (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; surround globally                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-evil-surround-mode 1)


;; Evil extension to integrate nicely with paredit
(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)

(provide 'evil-config)

;;; evil-config.el ends here
