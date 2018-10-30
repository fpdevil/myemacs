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
;;;
;;; Code:
;;;
;;;===========================================================================
(require 'cl)
(require 'web-mode)                                  ;; for all web/html/js related work
(require 'ac-emmet)                                  ;; emmet support
(require 'company-web-html)                          ;; load company mode html backend
(require 'company-web-jade)                          ;; load company mode jade backend
(require 'company-web-slim)                          ;; load company mode slim backend
;(require 'tidy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; an autonomous Emacs's major-mode for editing web templates                 ;;
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
(add-to-list 'auto-mode-alist '("\\.html\\'"                                  . web-mode))
(add-to-list 'auto-mode-alist '("\\.htm\\'"                                   . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"                                   . web-mode))
(add-to-list 'auto-mode-alist '("/\\(views\\|html\\|templates\\)/.*\\.php\\'" . web-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some customization for the web-mode                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-web-mode-hook ()
  "Customization for the web mode components."
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
        web-mode-attr-indent-offset 2
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

(after 'smartparens
  (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context)))


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
(defun turn-on-emmet-mode ()
  "Turn on the emmet mode."
  (require-package 'emmet-mode)
  (emmet-mode))
(add-hook 'sgml-mode-hook #'turn-on-emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  #'turn-on-emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook  #'turn-on-emmet-mode) ;; enable for web-mode

;; css indentation
(setq-default css-indent-offset 2)
;; 2 space indent also for element’s attributes, concatenations
;; and contiguous function calls
(with-eval-after-load 'web-mode
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

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


;; syntax highlighting
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

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


;; Enable JavaScript completion between <script>...</script> etc.
(defadvice company-tern (before web-mode-set-up-ac-sources activate)
  "Set `tern-mode' based on current language before running `company-tern'."
  (if (equal major-mode 'web-mode)
    (let ((web-mode-cur-language (web-mode-language-at-pos)))
      (if (or (string= web-mode-cur-language "javascript")
            (string= web-mode-cur-language "jsx"))
        (unless tern-mode (tern-mode))
        ;; (if tern-mode (tern-mode))
        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flymake-html-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "tidy" (list local-file))))

(defun flymake-html-load ()
  (interactive)
  (when (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
    (set (make-local-variable 'flymake-allowed-file-name-masks)
         '(("\\.html\\|\\.ctp\\|\\.ftl\\|\\.jsp\\|\\.php\\|\\.erb\\|\\.rhtml" flymake-html-init))
         )
    (set (make-local-variable 'flymake-err-line-patterns)
         ;; only validate missing html tags
         '(("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(missing <\/[a-z0-9A-Z]+>.*\\)" nil 1 2 4)))
    (flymake-mode 1)))

(defun web-mode-hook-setup ()
  (unless (aqua/is-buffer-file-temp)
    (flymake-html-load)
    (enable-flyspell-mode-conditionally)
    (setq flyspell-check-doublon nil)
    (remove-hook 'yas-after-exit-snippet-hook
                 'web-mode-yasnippet-exit-hook t)
    (remove-hook 'yas/after-exit-snippet-hook
                 'web-mode-yasnippet-exit-hook t)))

(add-hook 'web-mode-hook 'web-mode-hook-setup)

(after 'web-mode
  '(progn
     ;; make org-mode export fail, I use evil and evil-matchit
     ;; to select text, so expand-region.el is not used
     (remove-hook 'web-mode-hook 'er/add-web-mode-expansions)
     (setq web-mode-enable-auto-closing t) ; enable auto close tag in text-mode
     (setq web-mode-enable-auto-pairing t)
     (setq web-mode-enable-css-colorization t)
     (setq web-mode-imenu-regexp-list
           '(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
             ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1 " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
             ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1 " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")
             ;; angular imenu
             (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "=")))
     ))


;; auto-complete for html tags and attributes
(require-package 'ac-html)
;; auto complete angular15 data for `ac-html' and `company-web'
(require-package 'ac-html-angular)
;; Major modes for editing Angular 2
(require-package 'ng2-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'web-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; web-config.el ends here
