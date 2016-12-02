;;; package  --- web-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : web-config.el
;;; Description: Emacs configuration for Web development support
;;;              contains plugins and settings for json, javascript
;;;              html and css support using web-mode
;;;
;;; elisp code for customizing the web/html development settings
;;;===========================================================================
(require 'web-mode)                 ; for all web/html/js related work
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; an autonomous emacs major-mode for editing web templates                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.html?\\'"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'"    . web-mode))

(setq web-mode-enable-css-colorization t)
(setq web-mode-code-indent-offset 2)
(setq web-mode-markup-indent-offset 2)

(add-hook 'web-mode-hook
          (lambda ()
            (if (equal web-mode-content-type "javascript")
                (web-mode-set-content-type "jsx")
              (message "now set to: %s" web-mode-content-type))))

(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))


(defun web-mode-hook ()
  "Hooks for the web-mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-indent-style 2)
  )
(add-hook 'web-mode-hook  'web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))   ;; for js and jsx

(provide 'web-config)

;;; web-config.el ends here
