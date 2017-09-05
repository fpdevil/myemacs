;;; package --- elisp configuration settings for VIM
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename    : vim-config.el
;;; Description : A major mode for vim file handling in Emacs
;;;
;;; elisp code for vim language support and handling
;;;==========================================================================

;;;
;;; Code:
;;;

(use-package vimrc-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode)))

(provide 'vim-config)

;;; vim-config.el ends here
