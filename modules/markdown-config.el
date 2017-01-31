;;; package --- custom configuration for Markdown Mode
;;;
;;; Commentary:
;;; Filename   : markdown-config.el
;;; description: elisp code for customizing markdown mode for Emacs
;;;
;;;===========================================================================
(require 'markdown-mode)
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; editing files in markdown mode                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auto-mode-alist
    (append
        (list '("\\.text" . markdown-mode)
              '("\\.md" . markdown-mode)
              '("\\.mdml$" . markdown-mode)
              '("\\.markdown" . markdown-mode)
              )
        auto-mode-alist))

(provide 'markdown-config)
;;; markdown-config.el ends here
