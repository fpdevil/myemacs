;;; package  --- utils-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : utils-config.el
;;; Description: Some utilities and functions which does not have a proper
;;;              place or location
;;;              Using some functions from the excellent purcell Emacs.
;;; elisp code for customizing multiple things
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'switch-window)              ;; visual replacement for (C-x o)
(require-package 'pos-tip)
(require 'pos-tip)

(defun my-find-file-hook ()
  (unless (eq major-mode 'org-mode)
    (setq show-trailing-whitespace t))

  (when (string-match "\\.min\\." (buffer-file-name))
    (fundamental-mode))

  (visual-line-mode))
(add-hook 'find-file-hook #'my-find-file-hook)

;;----------------------------------------------------------------------------
;; imenu-list settings (invoke With "-bi")
;;----------------------------------------------------------------------------
(require 'golden-ratio)
(golden-ratio-mode 1)

(require 'imenu-list)
(add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*Ilist\\*")
(setq imenu-list-focus-after-activation t
      imenu-list-auto-resize t)

;;----------------------------------------------------------------------------
;; volatile-highlights
;;----------------------------------------------------------------------------
(require 'volatile-highlights)        ;; provide visual feedback
(volatile-highlights-mode t)

;;----------------------------------------------------------------------------
;; Emacs ElDoc: Display Function or Variable Information Near Point (Cursor)
;;----------------------------------------------------------------------------
(defun my-eldoc-display-message (format-string &rest args)
  "Display eldoc message near point."
  (when format-string
    (pos-tip-show (apply 'format format-string args))))
;; (setq eldoc-message-function #'my-eldoc-display-message)

;;----------------------------------------------------------------------------
;; Show tooltip with function documentation at point
;; by default clippy uses pos-tip - here we are using popup
;;----------------------------------------------------------------------------
(require 'clippy)
(setq clippy-tip-show-function #'clippy-popup-tip-show)

;;-----------------------------------------------------------------------------
;; toggle menu-bar, scroll-bar and tool-bar
;;-----------------------------------------------------------------------------
(defun aqua/toggle-bars ()
  "Toggles the menu, tool and scroll bars."
  (interactive)
  (if menu-bar-mode
      (progn
        (menu-bar-mode -1)
        (tool-bar-mode -1)
        (scroll-bar-mode -1))
    (progn
      (menu-bar-mode 1)
      (scroll-bar-mode 1)
      (tool-bar-mode 1))))

;;-----------------------------------------------------------------------------
;;; buffer mode
;;-----------------------------------------------------------------------------
(require 'buffer-move)
(global-set-key (kbd "<S-s-up>")     'buf-move-up)
(global-set-key (kbd "<S-s-down>")   'buf-move-down)
(global-set-key (kbd "<S-s-left>")   'buf-move-left)
(global-set-key (kbd "<S-s-right>")  'buf-move-right)

;;-----------------------------------------------------------------------------
;; get the buffer file name
;;-----------------------------------------------------------------------------
(defmacro buffer-real-name ()
  "Get the real filename of the current buffer without parent directory"
  '(file-name-nondirectory buffer-file-name))

;;-----------------------------------------------------------------------------
;; load any vendor packages
;;-----------------------------------------------------------------------------
(require 'dircolors)                  ;; ls --color inside emacs

;; pcomplete
(setq pcomplete-ignore-case t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'utils-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; utils-config.el ends here
