;;; package  --- utils-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : utils-config.el
;;; Description: Some utilities and functions which does not have a proper
;;;              place or location
;;;              Using some functions from the excellent purcell Emacs.
;;; elisp code for customizing multiple things
;;;==========================================================================
(require 'switch-window)              ;; visual replacement for (C-x o)
(require 'dircolors)                  ;; ls --color inside emacs
(require 'volatile-highlights)        ;; provide visual feedback

;;;
;;; Code:
;;;

;;---------------------------------------------------------------------------
(add-hook 'after-init-hook 'winner-mode)
;; navigate the window layouts using the below
;; "C-c <left>"
;; "C-c <right>"

(setq-default switch-window-shortcut-style 'alphabet) ;; shortcut with alphabet
(setq-default switch-window-timeout nil)              ;; control cancel switching after timeout

;; Usage shortcut
(global-set-key (kbd "C-x o") 'switch-window)

;; volatile-highlights
(volatile-highlights-mode t)

;;---------------------------------------------------------------------------
;; toggle menu-bar, scroll-bar and tool-bar
;;---------------------------------------------------------------------------
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

;;---------------------------------------------------------------------------
;;; buffer mode
;;---------------------------------------------------------------------------
(require 'buffer-move)
(global-set-key (kbd "<S-s-up>")     'buf-move-up)
(global-set-key (kbd "<S-s-down>")   'buf-move-down)
(global-set-key (kbd "<S-s-left>")   'buf-move-left)
(global-set-key (kbd "<S-s-right>")  'buf-move-right)

;;---------------------------------------------------------------------------
;; editor config
;;---------------------------------------------------------------------------
;;(use-package editorconfig
;;  :ensure t
;;  :config
;;  (editorconfig-mode 1))


;;---------------------------------------------------------------------------
;; get the buffer file name
;;---------------------------------------------------------------------------
(defmacro buffer-real-name ()
  "Get the real filename of the current buffer without parent directory"
  '(file-name-nondirectory buffer-file-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'utils-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; utils-config.el ends here
