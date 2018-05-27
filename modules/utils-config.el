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

;;* all utilities section
(lazy-init

 ;;{{{ window switching, the visual way
 ;; *visual* way to choose a window to switch (visual replacement for -> C-x o)
 ;;(require-package 'switch-window)
 ;;(require 'switch-window)
 ;;}}}

 (require-package 'pos-tip)
 (require 'pos-tip)

 (defun my-find-file-hook ()
   (unless (eq major-mode 'org-mode)
     (setq show-trailing-whitespace t))
   (when (string-match "\\.min\\." (buffer-file-name))
     (fundamental-mode)))
 (add-hook 'find-file-hook #'my-find-file-hook)

 ;;**
 ;; golden-ration and imenu-list settings (invoke With "-bi")
 (require-package 'golden-ratio)
 (golden-ratio-mode 1)

 (require-package 'imenu-list)
 (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*Ilist\\*")
 (setq imenu-list-focus-after-activation t
       imenu-list-auto-resize t)

 ;;**
 ;; HTTP REST client tool for emacs
 ;; C-c C-c: runs the query under the cursor, tries to pretty-print the response
 (require-package 'restclient)

 ;;**
 ;; volatile-highlights
 (require 'volatile-highlights)        ;; provide visual feedback
 (volatile-highlights-mode t)

;;**
;; hl-sexp: minor mode to highlight s-expression
(require-package 'hl-sexp)
(require 'hl-sexp)

(add-hook 'clojure-mode-hook #'hl-sexp-mode)
(add-hook 'lisp-mode-hook #'hl-sexp-mode)
;; (add-hook 'emacs-lisp-mode-hook #'hl-sexp-mode)

;;**
;; Emacs ElDoc: Display Function or Variable Information Near Point (Cursor)
(defun my-eldoc-display-message (format-string &rest args)
  "Display eldoc message near point."
  (when format-string
    (pos-tip-show (apply 'format format-string args))))
;; (setq eldoc-message-function #'my-eldoc-display-message)

;;**
;; Show tooltip with function documentation at point
;; by default clippy uses pos-tip - here we are using popup
(require 'clippy)
(setq clippy-tip-show-function #'clippy-popup-tip-show)

;;**
;; toggle menu-bar, scroll-bar and tool-bar
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

;;**
;; buffer mode
(require 'buffer-move)
(global-set-key (kbd "<S-s-up>")     'buf-move-up)
(global-set-key (kbd "<S-s-down>")   'buf-move-down)
(global-set-key (kbd "<S-s-left>")   'buf-move-left)
(global-set-key (kbd "<S-s-right>")  'buf-move-right)
;;**
;; rotate the layout of emacs
(require-package 'rotate)

;;**
;; get the buffer file name
(defmacro buffer-real-name ()
  "Get the real filename of the current buffer without parent directory"
  '(file-name-nondirectory buffer-file-name))

;;**
;; load any vendor packages
;; (require 'dircolors)                  ;; ls --color inside emacs

;;**
;; line numbers
;; (add-hook 'find-file-hook #'hl-line-mode)
(if (fboundp #'display-line-numbers-mode)
    (add-hook 'find-file-hook #'display-line-numbers-mode)
  (add-hook 'find-file-hook 'linum-mode))

;;**
;; a better package menu through paradox
(require-package 'paradox)
(require 'paradox)
(paradox-enable)
(setq paradox-execute-asynchronously t)

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'utils-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; utils-config.el ends here
