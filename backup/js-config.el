;;; package  --- js-config.el
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : js-config.el
;;; Description: Emacs configuration for javascript development support
;;; ref: http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;;; elisp code for customizing the js development settings
;;;===========================================================================
(require 'js2-refactor)             ;; javascript refactoring
(require 'js2-highlight-vars)       ;; highlight occurrences of vars
(require 'ac-js2)                   ;; javascript auto-completion in Emacs
(require 'js-doc)                   ;; insert JsDoc style comment easily
(require 'js2-mode)                 ;; js2 javascript mode
(require 'jsfmt)                    ;; formatting with jsfmt
(require 'indium)
(require 'node-ac-mode)             ;; nodejs auto-completion
(require 'json-mode)                ;; for json
(require 'json-navigator)           ;; view json docs (M-x json-navigator)
; (require 'simple-httpd)           ;; required and fulfilled by js2-mode
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Change some defaults: customize them to override                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst preferred-javascript-indent-level 2)
(setq-default js2-basic-offset 2
              js2-bounce-indent-p nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for js-mode indentation levels                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default js-indent-level preferred-javascript-indent-level)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file association and modes                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.js\\'"    . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js$"      . js2-mode))
;(add-to-list 'auto-mode-alist '("\\.es6\\'"   . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integrate with paredit                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key js-mode-map "{" 'paredit-open-curly)
(define-key js-mode-map "}" 'paredit-close-curly-and-newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; js2 and json modes
; https://truongtx.me/2014/02/23/set-up-javascript-development-environment-in-emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(js2-imenu-extras-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js2 variable highlight                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "js2-highlight-vars-autoloads"
  '(add-hook 'js2-mode-hook (lambda () (js2-highlight-vars-mode))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js2 syntax specific and key binding settings                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ac-js2-evaluate-calls t)
(setq js2-highlight-level 3        ;; amount of syntax highlighting to perform
      js2-use-font-lock-faces t
      js2-idle-timer-delay 0.5
      js2-auto-indent-p nil
      js2-indent-on-enter-key nil
      js2-skip-preprocessor-directives t
      js2-strict-inconsistent-return-warning nil
      js2-enter-indents-newline nil
      js2-bounce-indent-p t
      js2-auto-insert-catch-block t
      js2-cleanup-whitespace t
      js2-pretty-multiline-decl-indentation-p t
      js2-consistent-level-indent-inner-bracket-p t
      js2-global-externs (list "window" "module" "require"
                               "buster" "sinon" "assert"
                               "refute" "setTimeout" "clearTimeout"
                               "setInterval" "clearInterval" "location"
                               "__dirname" "console" "JSON" "jQuery" "$"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prettify symbols in javascript                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'js2-mode-hook
          (lambda ()
            (push '("function" . ?ƒ) prettify-symbols-alist)
            (push '("!==" . ?≠) prettify-symbols-alist)
            (push '("===" . ?≡) prettify-symbols-alist)
            (push '("&&" . ?∧) prettify-symbols-alist)
            (push '("||" . ?∨) prettify-symbols-alist)
            (push '("return" . ?η) prettify-symbols-alist)
            (push '("undefined" . ?Ս) prettify-symbols-alist)
            (push '("null" . ?∅) prettify-symbols-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; json-mode                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'json-mode-hook
          (lambda ()
            (setq js-indent-level 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; javascript code refactoring                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'js2-mode-hook #'js2-refactor-mode)
;; kbd prefix
(js2r-add-keybindings-with-prefix "C-c C-m")
;; extract function with `C-c C-m ef`
;; https://github.com/magnars/js2-refactor.el


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tern - intelligent javascript tooling                                    ;;
;; install with npm install -g tern                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'js-mode-hook
          (lambda ()
            (tern-mode t)
            (linum-mode 1))
          )
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup))
   )
(add-hook 'js2-mode-hook 'tern-mode)
(add-hook 'js-mode-hook 'tern-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Force restart of tern in new projects
;; $ M-x delete-tern-process
;; when Tern is not auto refreshing, fix the errors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun delete-tern-process ()
  "If Tern based auto-refresh is not happenning disable Tern."
  (interactive)
  (delete-process "Tern"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company auto completion for tern                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun start-tern-company ()
  "Start company Tern based auto completion for js."
  (interactive)
  (eval-after-load 'tern
    '(progn
       (add-to-list 'company-backends 'company-tern))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code beautification through web-beautify and js-beautify npm package     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
(eval-after-load 'js
  '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax checking and linting with jsxhint                                 ;;
;; install with npm install -g jsxhint                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (require 'flycheck)

  ;; turn on flycheck globally
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)

  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)

  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")

  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint)))

  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(json-jsonlist))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck syntax checking                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for js evaluation in js buffers through indium (formerly jade)           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-hook 'js2-mode-hook #'jade-interaction-mode)
(add-hook 'js2-mode-hook #'indium-interaction-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js3-mode settings (beefed up js2-mode)                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'js3-mode-hook
          (lambda ()
            (make-variable-buffer-local 'tab-width)
            (make-variable-buffer-local 'indent-tabs-mode)
            (make-variable-buffer-local 'whitespace-style)
            (wrap-region-mode 1)
            (hs-minor-mode 1)
            (rainbow-mode 1)
            (moz-minor-mode 1)
            (setq mode-name "js3")
            (add-hook 'before-save-hook 'whitespace-cleanup nil 'local)
            (setq js3-use-font-lock-faces t)
            (setq js3-mode-must-byte-compile nil)
            (setq js3-basic-offset preferred-javascript-indent-level)
            (setq js3-indent-on-enter-key t)
            (setq js3-auto-indent-p t)
            (setq js3-curly-indent-offset 0)
            (setq js3-expr-indent-offset 2)
            (setq js3-lazy-commas t)
            (setq js3-laxy-dots t)
            (setq js3-lazy-operators t)
            (setq js3-paren-indent-offset 2)
            (setq js3-square-indent-offset 4)
            (setq js3-enter-indents-newline t)
            (setq js3-bounce-indent-p nil)
            (setq js3-auto-insert-catch-block t)
            (setq js3-cleanup-whitespace t)
            (setq js3-global-externs '(Ext console))
            (setq js3-highlight-level 3)
            (setq js3-mirror-mode t) ; conflicts with autopair
            (setq js3-mode-escape-quotes t) ; t disables
            (setq js3-mode-squeeze-spaces t)
            (setq js3-pretty-multiline-decl-indentation-p t)
            (setq js3-consistent-level-indent-inner-bracket-p t)
            (setq js3-rebind-eol-bol-keys t)
            (setq js3-indent-tabs-mode t)
            (setq js3-compact-list t)
            (setq js3-compact-while t)
            (setq js3-compact-infix t)
            (setq js3-compact-if t)
            (setq js3-compact-for t)
            (setq js3-compact-expr t)
            (setq js3-compact-case t)
            (setq js3-case t)
            (setq
             tab-width 2
             js3-basic-offset 2
             indent-tabs-mode t
             whitespace-style '(face tabs spaces trailing lines space-before-tab::tab newline indentation::tab empty space-after-tab::tab space-mark tab-mark newline-mark)
             )
            )
            (linum-mode 1))
(add-to-list 'ac-modes 'js3-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; live loading with skewer (for front-end)                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsfmt For formatting, searching, and rewriting javascript                ;;
;; prerequisite npm install -g jsfmt                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'before-save-hook 'jsfmt-before-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(add-hook 'js2-mode-hook
          (lambda ()
            (flycheck-select-checker 'javascript-eslint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extra setup                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "js2-mode"
  '(progn
     (setq js2-missing-semi-one-line-override t)
     (setq-default js2-basic-offset 2)
     ;; 2 spaces for indentation (if you prefer 2 spaces instead of default 4 spaces for tab)
     ;; add from jslint global variable declarations to js2-mode globals list
     ;; modified from one in http://www.emacswiki.org/emacs/Js2Mode
     (defun my-add-jslint-declarations ()
       (when (> (buffer-size) 0)
         (let ((btext (replace-regexp-in-string
                       (rx ":" (* " ") "true") " "
                       (replace-regexp-in-string
                        (rx (+ (char "\n\t\r "))) " "
                        ;; only scans first 1000 characters
                        (save-restriction (widen) (buffer-substring-no-properties (point-min) (min (1+ 1000) (point-max)))) t t))))
           (mapc (apply-partially 'add-to-list 'js2-additional-externs)
                 (split-string
                  (if (string-match (rx "/*" (* " ") "global" (* " ") (group (*? nonl)) (* " ") "*/") btext)
                      (match-string-no-properties 1 btext) "")
                  (rx (* " ") "," (* " ")) t))
           )))
     (add-hook 'js2-post-parse-callbacks 'my-add-jslint-declarations)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js-doc JsDoc style documentation and comments                            ;;
;; 1. insert function document by pressing Ctrl + c, i                      ;;
;; 2. insert @tag easily by pressing @ in the JsDoc style comment           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq js-doc-mail-address "Sampath.Singamsetty"
      js-doc-author (format "Sampath Singamsetty" js-doc-mail-address)
      js-doc-url "http://united.com"
      js-doc-license "license name")

(add-hook 'js2-mode-hook
          #'(lambda ()
              (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
              (define-key js2-mode-map "@" 'js-doc-insert-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-ac node js auto-completion for emacs                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-hook 'js2-mode-hook
;;           (lambda ()
;; 		    (local-set-key (kbd "C-.") 'node-ac-auto-complete)
;; 		  	(local-set-key (kbd "C-c C-d") 'node-ac-show-document)
;; 		  	(local-set-key (kbd "C-c C-j") 'node-ac-jump-to-definition)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'js-config)
;;; js-config.el ends here
