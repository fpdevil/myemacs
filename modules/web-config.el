;;; package  --- web-config.el
;;;
;;; Commentary:
;;;
;;; Filename.  : web-config.el
;;; Description: Emacs configuration for Web development support
;;;              contains plugins and settings for json, javascript
;;;              html and css support using web-mode
;;;
;;; elisp code for customizing the web/html development settings
;;;==============================================================
(require 'web-mode)
;
;;; Code:
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; an autonomous emacs major-mode for editing web templates                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-enable-css-colorization t)
(setq web-mode-code-indent-offset 2)
(setq web-mode-markup-indent-offset 2)

(provide 'web-config)

;;; web-config.el ends here
