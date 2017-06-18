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
(require 'web-mode)                                  ;; for all web/html/js related work
(require 'ac-emmet)                                  ;; emmet support
(require 'company-web-html)                          ;; load company mode html backend
(require 'company-web-jade)                          ;; load company mode jade backend
(require 'company-web-slim)                          ;; load company mode slim backend
(require 'indium)
;(require 'tidy)
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; an autonomous emacs major-mode for editing web templates                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'"                             . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'"                                   . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'"                               . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'"                                   . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'"                              . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$\\'"                                  . web-mode))    ;; for jsx
(add-to-list 'auto-mode-alist '("\\.es6\\'"                                   . web-mode))    ;; for ES6 js
(add-to-list 'auto-mode-alist '("\\.phtml\\'"                                 . web-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'"                                   . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'"                           . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'"                                   . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'"                               . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'"                              . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'"                                . web-mode))
(add-to-list 'auto-mode-alist '("\\.*tpl\\'"                                  . web-mode))
(add-to-list 'auto-mode-alist '("\\.*tml\\'"                                  . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'"                                 . web-mode))
(add-to-list 'auto-mode-alist '("/\\(views\\|html\\|templates\\)/.*\\.php\\'" . web-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some customizations for the web-mode                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-web-mode-hook ()
  "Customizations for the web mode components."
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-opening t
        web-mode-enable-auto-indentation t
        web-mode-enable-block-face t
        web-mode-enable-part-face t
        web-mode-enable-comment-keywords t
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t
        web-mode-enable-heredoc-fontification t
        web-mode-enable-engine-detection t
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-style-padding 2
        web-mode-script-padding 2
        web-mode-block-padding 0
        web-mode-comment-style 2
        web-mode-enable-auto-pairing nil)              ;; for making web-mode work with smartparens
  )
(add-hook 'web-mode-hook 'my-web-mode-hook)

(defun sp-web-mode-is-code-context (id action context)
  "Smart parens support ID ACTION CONTEXT."
  (and (eq action 'insert)
       (not (or (get-text-property (point) 'part-side)
                (get-text-property (point) 'block-side)))))

(sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; associate web-mode engines                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq web-mode-engines-alist
      '(
        ("php" . "\\.phtml\\'")
        ("blade" . "\\.blade\\'")
        ("django" . "\\.[sd]tpl\\'")
        ("django" . "\\.[sd]tml\\'")
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customize the colors for syntax highlighting                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 '(web-mode-html-tag-face
   ((t (:foreground "#729fcf"))))
 '(web-mode-html-attr-name-face
   ((t (:foreground "#C97586"))))
 '(web-mode-html-attr-value-face
   ((t (:foreground "#82AE46"))))
 '(web-mode-html-tag-bracket-face
   ((t (:foreground "#FFE84B"))))
 '(web-mode-comment-face
   ((t (:foreground "#D9333F"))))
 '(web-mode-server-comment-face
   ((t (:foreground "#D9333F"))))
 '(web-mode-current-element-highlight-face
   ((t (:foreground "#FF8A4B"))))
 '(web-mode-current-element-highlight-face
   ((t (:background "#000000"
                    :foreground "#FF8A4B")))))


(set-face-attribute 'web-mode-doctype-face nil :foreground "SlateBlue")
(set-face-attribute 'web-mode-html-tag-face nil :foreground "MediumBlue")
(set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "red")
(set-face-attribute 'web-mode-html-attr-name-face nil :foreground "orange")
(set-face-attribute 'web-mode-css-at-rule-face nil :foreground "Pink3")
(set-face-attribute 'web-mode-variable-name-face nil :foreground "DarkGreen")
(set-face-attribute 'web-mode-comment-face nil :foreground "red")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company web completions                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(setq company-idle-delay .5)                        ;; decrease delay before autocompletion popup shows
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(global-set-key (kbd "C-c /") 'company-files)        ; Force complete file names on "C-c /" key
;; use company-mode with company-web-html in web-mode
(add-hook 'web-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-web-html))
                          (company-mode t)))

;; html document support
;(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;; set up HTML mode to do indenting
(add-hook 'html-mode-hook
    (lambda ()
    (setq indent-line-function 'indent-relative)))
;; HTML settings
;; set up HTML mode to do indenting
(add-hook 'html-mode-hook
          (lambda ()
            (setq indent-line-function 'indent-relative)))

;; for html support through tidy
;; (add-hook 'html-mode-hook (lambda () (tidy-build-menu html-mode-map)))

;; auto complete with ac-emmet through auto-complete and emmet
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; snippet and auto-completions                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-css-primary))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))
        ("php" . (ac-source-words-in-buffer
                  ac-source-words-in-same-mode-buffers
                  ac-source-dictionary))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax checking install with npm install -g jsxhint                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if eslint is needed instead of jsxhint                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(flycheck-add-mode 'javascript-eslint 'web-mode)

;;disable jshint since we prefer eslint checking
(defun switch-eslint ()
  "Disable jsxhint and set eslint."
  (interactive)
  (require 'flycheck)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (web-mode)
  (web-mode-set-content-type "jsx")
  (flycheck-select-checker 'javascript-eslint)
  (flycheck-mode)
  (tern-mode t))

;; (add-to-list 'magic-mode-alist '("import " . switch-eslint) )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; code beautification through web-beautify and js-beautify npm package
(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'web-mode
  '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'web-config)
;;; web-config.el ends here
