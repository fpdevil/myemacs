;;; clr-identifiers-config.el --- Emacs configuration settings for color identifiers
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : clr-identifiers-config.el
;;; Description: Emacs minor mode to highlight each source code identifier
;;;              uniquely based on its name
;;;
;;; Code:
;;;

(when (eq dotemacs-clr-identifiers 'color-identifiers)
  (require-package 'color-identifiers-mode)

  (let ((faces '(font-lock-comment-face
                 font-lock-comment-delimiter-face
                 font-lock-constant-face
                 font-lock-type-face
                 font-lock-function-name-face
                 font-lock-variable-name-face
                 font-lock-keyword-face
                 font-lock-string-face
                 font-lock-builtin-face
                 font-lock-preprocessor-face
                 font-lock-warning-face
                 font-lock-doc-face)))
    (dolist (face faces)
      (set-face-attribute face nil :foreground nil :weight 'normal :slant 'normal)))

  (set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'italic)
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-constant-face nil :foreground nil :underline t)
  (set-face-attribute 'font-lock-type-face nil :foreground nil)
  (set-face-attribute 'font-lock-function-name-face nil :foreground nil :weight 'bold)
  (set-face-attribute 'font-lock-variable-name-face nil :foreground nil)
  (set-face-attribute 'font-lock-keyword-face nil :foreground nil :slant 'italic)
  (set-face-attribute 'font-lock-string-face nil :background "#e0e0e0")
  (set-face-attribute 'font-lock-doc-face nil :slant 'italic)
  (set-face-attribute 'font-lock-builtin-face nil :foreground nil :slant 'italic)
  (set-face-attribute 'font-lock-preprocessor-face nil :weight 'bold)

  (add-hook 'after-init-hook 'global-color-identifiers-mode)
  ;;(add-hook 'prog-mode-hook 'color-identifiers-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'clr-identifiers-config)

;;; clr-identifiers-config.el ends here
