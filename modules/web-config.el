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
;;;

(require 'web-mode)                                  ;; for all web/html/js related work
(require 'company-web-html)                          ;; load company mode html backend
(require 'company-web-jade)                          ;; load company mode jade backend
(require 'company-web-slim)                          ;; load company mode slim backend
(require 'json-navigator)                            ;; view and navigate JSON structures
;(require 'tidy)

;;-----------------------------------------------------------------------------
;; an autonomous Emacs's major-mode for editing web templates
;;-----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.\\(cmp\\|app\\|page\\|component\\|es6\\)\\'"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.\\(wp\\|vue\\|tmpl\\|php\\|blade\\|module\\)\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.\\(inc\\|hbs\\|tpl\\|tml\\|[gj]sp\\|as[cp]x\\)\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.\\(mustache\\|djhtml\\|html\\|ftl\\)\\'"           . web-mode))
(add-to-list 'auto-mode-alist '("\\.\\([rp]?html?\\|html\\|xul?\\|eex?\\|xml?\\)\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.\\(jst\\|ejs\\|erb\\|views\\|templates\\)\\'"      . web-mode))


;;-----------------------------------------------------------------------------
;; flymake error handling
;;-----------------------------------------------------------------------------
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
         '(("\\.html\\|\\.ctp\\|\\.ftl\\|\\.jsp\\|\\.php\\|\\.erb\\|\\.rhtm\\|\\.vue" flymake-html-init))
         )
    (set (make-local-variable 'flymake-err-line-patterns)
         ;; only validate missing html tags
         '(("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(missing <\/[a-z0-9A-Z]+>.*\\)" nil 1 2 4)))
    (flymake-mode 1)))

(defun web-mode-hook-setup ()
  (unless (is-buffer-file-temp)
    (flymake-html-load)
    (enable-flyspell-mode-conditionally)
    (setq flyspell-check-doublon nil)
    (remove-hook 'yas-after-exit-snippet-hook
                 'web-mode-yasnippet-exit-hook t)
    (remove-hook 'yas/after-exit-snippet-hook
                 'web-mode-yasnippet-exit-hook t)))

(add-hook 'web-mode-hook 'web-mode-hook-setup)

;;-----------------------------------------------------------------------------
;; some customization for the web-mode
;;-----------------------------------------------------------------------------
(eval-after-load 'web-mode
  '(progn
     ;; make org-mode export fail, I use evil and evil-matchit
     ;; to select text, so expand-region.el is not used
     (remove-hook 'web-mode-hook 'er/add-web-mode-expansions)
     (setq web-mode-enable-auto-closing t) ; enable auto close tag in text-mode
     (setq web-mode-enable-auto-pairing t)
     (setq web-mode-enable-css-colorization t)
     (setq web-mode-enable-auto-opening t)
     (setq web-mode-enable-auto-indentation t)
     (setq web-mode-enable-block-face t)
     (setq web-mode-enable-part-face t)
     (setq web-mode-enable-comment-keywords t)
     (setq web-mode-enable-current-element-highlight t)
     (setq web-mode-enable-current-column-highlight t)
     (setq web-mode-enable-heredoc-fontification t)
     (setq web-mode-enable-engine-detection t)
     (setq web-mode-markup-indent-offset 2)
     (setq web-mode-css-indent-offset 2)
     (setq web-mode-code-indent-offset 2)
     (setq web-mode-attr-indent-offset 2)
     (setq web-mode-style-padding 2)
     (setq web-mode-script-padding 2)
     (setq web-mode-block-padding 0)
     (setq web-mode-comment-style 2)
     (setq web-mode-imenu-regexp-list
           '(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
             ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1 " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
             ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1 " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")
             ;; angular imenu
             (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "=")))))

(defun sp-web-mode-is-code-context (id action context)
  "Smart parens support ID ACTION CONTEXT."
  (and (eq action 'insert)
       (not (or (get-text-property (point) 'part-side)
                (get-text-property (point) 'block-side)))))

(after 'smartparens
  (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context)))


;;-----------------------------------------------------------------------------
;; customize the colors for syntax highlighting
;;-----------------------------------------------------------------------------
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

(when (eq 'dotemacs-completion-engine 'auto-complete)
  ;; emmet support
  (require 'ac-emmet)
  ;; auto-complete for html tags and attributes
  (require-package 'ac-html)
  ;; auto complete angular15 data for `ac-html' and `company-web'
  (require-package 'ac-html-angular))
;; major modes for editing Angular 2
(require-package 'ng2-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'web-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; web-config.el ends here
