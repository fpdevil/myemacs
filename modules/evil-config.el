;;; package --- evil-config.el configuration settings for vim emulation
;;;
;;; Commentary:
;;;
;;; Filename   : evil-config.el
;;; Description: Emacs Vim Emulation Mode or evil
;;;
;;; elisp code for customizing the evil
;;; undo-tree: undo (C-/) behaves just like normal editor.  To redo, C-_
;;; To open the undo tree, C-x u
;;===========================================================================
(require 'evil)
(require 'undo-tree)
(require 'evil-leader)
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil mode (for vim emuation)                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-mode t)                                   ;; enable evil-mode globally
(global-undo-tree-mode)                         ;; enable undo-tree globally

;; evil search
(setq evil-search-module 'evil-search)
(setq evil-magic 'very-magic)

;;
; change cursor colors based on the mode
;;
(setq evil-emacs-state-cursor    '("red" box))
(setq evil-motion-state-cursor   '("orange" box))
(setq evil-normal-state-cursor   '("green" box))
(setq evil-visual-state-cursor   '("orange" box))
(setq evil-insert-state-cursor   '("red" bar))
(setq evil-replace-state-cursor  '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

(add-hook 'evil-jumps-post-jump-hook #'recenter)

;;
; evil operations for various vim states
;;
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


;; bind all emacs-state key to insert state
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map
  (read-kbd-macro evil-toggle-key) 'evil-emacs-state)

;; make sure ESC key in insert-state will call evil-normal-state
(define-key evil-insert-state-map [escape] 'evil-normal-state)

;; make all emacs-state buffer become to insert-state
(dolist (m evil-emacs-state-modes)
  (add-to-list 'evil-insert-state-modes m))

;; enable evil-leader globally
(global-evil-leader-mode)


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

;;
; surround globally
;;
(global-evil-surround-mode 1)

(provide 'evil-config)

;;; evil-config.el ends here
