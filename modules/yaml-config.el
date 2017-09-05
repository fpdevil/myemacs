;;; yaml-config.el --- Emacs configuration settings for editing YAML format files
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : yaml-config.el
;;; Description: Emacs configuration for yaml development support
;;;
;;;===========================================================================

;;;
;;; Code:
;;;

(use-package yaml-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'yaml-config)

;;; yaml-config.el ends here
