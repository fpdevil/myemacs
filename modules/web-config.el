;;; package  --- web-config.el
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : web-config.el
;;; Description: Emacs configuration for Web development support
;;;              contains plugins and settings for json, javascript
;;;              html and css support using web-mode
;;;              https://truongtx.me/
;;;
;;; elisp code for customizing the web/html development settings
;;;===========================================================================
(require 'cl)
(require 'web-mode)                 ; for all web/html/js related work
(require 'flycheck)                 ; on the fly syntax checker
(require 'js2-refactor)             ; javascript refactoring
(require 'ac-js2)                   ; javascript auto-completion in Emacs
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
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'"    . web-mode))   ;; for js and jsx
(add-to-list 'auto-mode-alist '("\\.es6\\'"       . web-mode))   ;; for ES6 js


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

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  "Highlighting the components."
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;;
; js2 mode
; https://truongtx.me/2014/02/23/set-up-javascript-development-environment-in-emacs
;;
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)                ;; amount of syntax highlighting to perform

;;
; javascript code refactoring
;;
(add-hook 'js2-mode-hook #'js2-refactor-mode)


;;
; tern - intelligent javascript tooling
; install with npm install -g tern
;;
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

;; Fix error when Tern does not auto refresh
(defun delete-tern-process ()
  "When auto-refresh is not happenning disable tern."
  (interactive)
  (delete-process "Tern"))


;;
; syntax checking
; install with npm install -g jsxhint
;;
(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode))))

;;
; flycheck syntax checking
;;
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))


(provide 'web-config)

;;; web-config.el ends here
