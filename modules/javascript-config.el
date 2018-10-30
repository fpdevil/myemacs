;;; package  --- js-config.el
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : js-config.el
;;; Description: Emacs configuration for javascript development support
;;; ref: http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;;; elisp code for customizing the js development settings
;;; -- flycheck --
;;; javascript-eslint is  part of flycheck, it  will fallback to it  as we
;;; have  disabled  priority   one  javascript-jshint  and  json-json-lint
;;; checkers
;;;
;;; Code:
;;;
;;;=============================================================================
(require 'js2-highlight-vars)       ;; highlight occurrences of vars
(require 'ac-js2)                   ;; javascript auto-completion in Emacs
(require 'js-doc)                   ;; insert JsDoc style comment easily
(require 'js2-mode)                 ;; js2 javascript mode
(require 'jsfmt)                    ;; formatting with jsfmt
(require 'json-mode)                ;; for json
(require 'json-navigator)           ;; view json docs (M-x json-navigator)

;;(require 'tern-context-coloring)
;;(require 'simple-httpd)           ;; required and fulfilled by js2-mode

;;------------------------------------------------------------------------------
;; Change some defaults: customize them to override (js-mode indentation levels)
;;------------------------------------------------------------------------------
(defconst preferred-javascript-indent-level 4)
(setq-local js-indent-level preferred-javascript-indent-level)
(setq-local js2-basic-offset preferred-javascript-indent-level)
(setq-local javascipt-indent-level preferred-javascript-indent-level)

;;------------------------------------------------------------------------------
;; json-mode indentation settings
;;------------------------------------------------------------------------------
(add-hook 'json-mode-hook
          (lambda ()
            (setq js-indent-level preferred-javascript-indent-level)))

;;------------------------------------------------------------------------------
;; integration with paredit and smartparens
;;------------------------------------------------------------------------------
;; (after "paredit"
;; (define-key js-mode-map "{" 'paredit-open-curly)
;; (define-key js-mode-map "}" 'paredit-close-curly-and-newline))

;; newline and indentation after {}
(sp-local-pair 'js-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(sp-local-pair 'js2-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(sp-local-pair 'javascript-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

;; make return key also do indent, for current buffer only
(electric-indent-local-mode 1)

;;------------------------------------------------------------------------------
;; file modes association and auto lists
;;------------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.js\\'"       . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js$"         . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'"      . js2-mode))    ;; for jsx
(add-to-list 'auto-mode-alist '("\\.eslintrc.*$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.babelrc$"    . json-mode))

(add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))
;; (add-to-list 'interpreter-mode-alist '("node"    . js2-mode))

;;------------------------------------------------------------------------------
; js2 and json modes
; https://truongtx.me/2014/02/23/set-up-javascript-development-environment-in-emacs
;;------------------------------------------------------------------------------
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js2-mode-hook 'ac-js2-setup-auto-complete-mode)

;; for better imenu support, enable the extras
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

;;------------------------------------------------------------------------------
;; js2 variable highlighting where applicable
;;------------------------------------------------------------------------------
(after "js2-highlight-vars-autoloads"
  '(add-hook 'js2-mode-hook (lambda () (js2-highlight-vars-mode))))

;;------------------------------------------------------------------------------
;; js2 customizations - syntax specific and key binding settings
;;------------------------------------------------------------------------------
(setq ac-js2-evaluate-calls t)
(setq js2-highlight-level 3        ;; amount of syntax highlighting to perform
      js2-use-font-lock-faces t
      js2-idle-timer-delay 0.1
      js2-auto-indent-p nil
      js2-indent-on-enter-key nil
      js2-enter-indents-newline nil
      js2-bounce-indent-p t
      js2-skip-preprocessor-directives t
      js2-strict-inconsistent-return-warning nil
      js2-auto-insert-catch-block t
      js2-cleanup-whitespace t
      js2-pretty-multiline-decl-indentation-p t
      js2-consistent-level-indent-inner-bracket-p t
      js2-global-externs (list "window" "module" "require"
                               "buster" "sinon" "assert"
                               "refute" "setTimeout" "clearTimeout"
                               "setInterval" "clearInterval" "location"
                               "__dirname" "console" "JSON" "jQuery" "$" "angular"))

;;------------------------------------------------------------------------------
;; tern - intelligent javascript tooling
;; first install with npm - install -g tern
;;------------------------------------------------------------------------------
(after "js2-mode-autoloads"
  ;; javascript code refactoring
  ;; https://github.com/magnars/js2-refactor.el
  (require-package 'js2-refactor)
  (after 'js2-refactor
    ;; kbd prefix as C-c C-m (extract function with `C-c C-m ef`)
    (js2r-add-keybindings-with-prefix "C-c C-m"))

  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (add-hook 'js2-minor-mode-hook #'js2-refactor-mode)

  (when (executable-find "tern")
    (require-package 'tern)
    (after 'tern
      (after 'auto-complete
        (require-package 'tern-auto-complete)
        (tern-ac-setup))
      (after 'company-mode
             (require-package 'company-tern)
             (defun tern-company-backends ()
               "Set company backends for tern."
               (make-local-variable 'company-backends)
               ;; (setq company-backends '((company-etags company-capf company-dabbrev company-tern)))
               (push 'company-etags company-backends)
               (push 'company-capf company-backends)
               (push 'company-dabbrev company-backends)
               (push 'company-tern company-backends)
               )
             ;; (add-to-list 'company-backends 'company-tern)
             (add-hook 'js2-mode-hook (lambda ()
                                        (tern-mode)
                                        (company-mode)))
             (add-hook 'tern-mode-hook 'tern-company-backends)
             ))

    (add-hook 'js2-mode-hook #'tern-mode)
    (add-hook 'js2-minor-mode-hook #'tern-mode)))

(add-hook 'js-mode-hook
  (lambda ()
    (tern-mode t)))

;; company auto completion integration with tern
;; (after 'company
;;   (when (executable-find "tern")
;;     (after "company-tern-autoloads"
;;       (add-to-list (make-local-variable 'company-backends) 'company-tern))))

;; tern completion with auto complete mode
;; (after "auto-complete"
;;    '(progn
;;       (require 'tern-auto-complete)
;;       (tern-ac-setup)))


;; (add-hook 'js2-mode-hook
;;   (lambda ()
;;     (tern-mode t)))

;;------------------------------------------------------------------------------
;; Force restart of tern in new projects (M-x delete-tern-process)
;; If we have just added the .tern-project file or edit the file but Tern
;; does not auto reload, we need to manually kill Tern server
;;------------------------------------------------------------------------------
(defun delete-tern-process ()
  "If Tern based auto-refresh is not happening disable Tern."
  (interactive)
  (delete-process "Tern"))

;;------------------------------------------------------------------------------
;; code beautification through web-beautify and js-beautify npm package
;;------------------------------------------------------------------------------
(after 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
(after 'js
  '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))
;; for json mode files
(after 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

;;------------------------------------------------------------------------------
;; js3-mode settings (beefed up js2-mode)
;;------------------------------------------------------------------------------
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
             whitespace-style '(face
                                tabs
                                spaces
                                trailing
                                lines
                                space-before-tab::tab newline
                                indentation::tab empty
                                space-after-tab::tab space-mark tab-mark newline-mark))))
(add-to-list 'ac-modes 'js3-mode)

;;------------------------------------------------------------------------------
;; [ skewer ] - live loading with skewer (for front-end)
;; [ livid ] - Live browser eval of JavaScript every time a buffer changes
;;------------------------------------------------------------------------------
(after "skewer-mode"
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

(require-package 'livid-mode)
(require 'livid-mode)

;;------------------------------------------------------------------------------
;; jsfmt For formatting, searching, and rewriting javascript
;; prerequisite npm install -g jsfmt
;;------------------------------------------------------------------------------
(add-hook 'before-save-hook 'jsfmt-before-save)

;;------------------------------------------------------------------------------
;; additional setup for indentation
;;------------------------------------------------------------------------------
(after "js2-mode"
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
                        (save-restriction
                          (widen)
                          (buffer-substring-no-properties
                           (point-min)
                           (min (1+ 1000) (point-max)))) t t))))
           (mapc (apply-partially 'add-to-list 'js2-additional-externs)
                 (split-string
                  (if (string-match
                       (rx "/*" (* " ") "global" (* " ") (group (*? nonl)) (* " ") "*/") btext)
                      (match-string-no-properties 1 btext) "")
                  (rx (* " ") "," (* " ")) t))
           )))
     (add-hook 'js2-post-parse-callbacks 'my-add-jslint-declarations)))

;;------------------------------------------------------------------------------
;; js-doc JsDoc style documentation and comments
;; 1. insert function document by pressing Ctrl + c, i
;; 2. insert @tag easily by pressing @ in the JsDoc style comment
;;------------------------------------------------------------------------------
(setq js-doc-mail-address "Sampath.Singamsetty"
      js-doc-author (format "Sampath Singamsetty" js-doc-mail-address)
      js-doc-url "http://united.com"
      js-doc-license "license name")

(add-hook 'js2-mode-hook
          #'(lambda ()
              (define-key js2-mode-map "\C-c i" 'js-doc-insert-function-doc)
              (define-key js2-mode-map "@" 'js-doc-insert-tag)))

;;------------------------------------------------------------------------------
;; key bindings for node-ac node js auto-completion for emacs
;;------------------------------------------------------------------------------
; (require 'node-ac-mode)             ;; nodejs auto-completion
; (setq node-ac-node-modules-path "/usr/local/lib/node_modules")

;; [optional] - key bindings
;; (add-hook 'js2-mode-hook
;;           (lambda ()
;;        (local-set-key (kbd "C-.") 'node-ac-auto-complete)
;;        (local-set-key (kbd "C-c C-d") 'node-ac-show-document)
;;        (local-set-key (kbd "C-c C-j") 'node-ac-jump-to-definition)))

;;------------------------------------------------------------------------------
;; syntax checking and linting with jsxhint
;; first install checker using package manager - npm install -g jsxhint
;;------------------------------------------------------------------------------
(after "flycheck"
  '(progn
    ;; (require 'flycheck)

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
        '(json-jsonlist)))))

;;------------------------------------------------------------------------------
;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
;;------------------------------------------------------------------------------
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable (executable-find "eslint")))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; flycheck  will use  eslint and  then stop  with collecting  errors and
;; warnings. jscs  is never called  if eslint  is present. In  most modes
;; flycheck  will  chain  checkers,  one   after  another,  but  not  for
;; JavaScript.
(after 'flycheck
  ;; define checker for jscs
  (flycheck-define-checker javascript-jscs
    "Define a JavaScript code style checker.
Refer the URL `https://github.com/mdevils/node-jscs'."
    :command ("jscs" "--reporter=checkstyle"
              (config-file "--config" flycheck-jscs)
              source)
    :error-parser flycheck-parse-checkstyle
    :modes (js-mode js2-mode js3-mode)
    :next-checkers (javascript-jshint))

  ;; flycheck jscs configuration file
  (flycheck-def-config-file-var flycheck-jscs javascript-jscs ".jscsrc"
    :safe #'stringp)

  ;; make javascript-jscs automatically selectable to flycheck
  ;; Use t to append at the end so it's not used by default.
  (add-to-list 'flycheck-checkers 'javascript-jscs t)
  ;; hain javascript-jscs to run after javascript-jshint
  (flycheck-add-next-checker 'javascript-eslint '(t . javascript-jscs)))

(defun jscs-enable ()
  "Enable the jscs checker."
  (interactive)
       (add-to-list 'flycheck-checkers 'javascript-jscs))

(defun jscs-disable ()
  "Disable the jscs checker."
  (interactive)
       (setq flycheck-checkers (remove 'javascript-jscs flycheck-checkers)))

;;------------------------------------------------------------------------------
;; flycheck with jslint (npm install jslinter -g)
;;------------------------------------------------------------------------------
(defun my-parse-jslinter-warning (warning)
  (flycheck-error-new
   :line (1+ (cdr (assoc 'line warning)))
   :column (1+ (cdr (assoc 'column warning)))
   :message (cdr (assoc 'message warning))
   :level 'debug
   :buffer (current-buffer)
   :checker 'javascript-jslinter))

(defun jslinter-error-parser (output checker buffer)
  (mapcar 'parse-jslinter-warning
          (cdr (assoc 'warnings (aref (json-read-from-string output) 0)))))
(flycheck-define-checker javascript-jslinter
  "A JavaScript syntax and style checker based on JSLinter.
See URL `https://github.com/tensor5/JSLinter'."
  :command ("jslint" "--raw" source)
  :error-parser jslinter-error-parser
  :modes (js-mode js2-mode js3-mode))

;;------------------------------------------------------------------------------
;; [ flycheck ] syntax checker
;;------------------------------------------------------------------------------
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

;;------------------------------------------------------------------------------
;; [ flymake ] syntax checker for json  mode
;;------------------------------------------------------------------------------
(require-package 'flymake-json)
(add-hook 'json-mode 'flymake-json-load)

;;------------------------------------------------------------------------------
;;; [ import-js ] -- A tool to simplify importing of JS modules.
;;------------------------------------------------------------------------------
(require-package 'import-js)
(require 'import-js)

;;------------------------------------------------------------------------------
;; prettify symbols in javascript
;;------------------------------------------------------------------------------
(add-hook 'js2-mode-hook
          (lambda ()
            (push '("function"  . ?ƒ) prettify-symbols-alist)
            (push '("!=="       . ?≠) prettify-symbols-alist)
            (push '("==="       . ?≡) prettify-symbols-alist)
            (push '("&&"        . ?∧) prettify-symbols-alist)
            (push '("||"        . ?∨) prettify-symbols-alist)
            (push '("return"    . ?η) prettify-symbols-alist)
            (push '("undefined" . ?Ս) prettify-symbols-alist)
            (push '("null"      . ?∅) prettify-symbols-alist)))

;;------------------------------------------------------------------------------
;; tern plugin adding scope coloring
;;------------------------------------------------------------------------------
;; (eval-after-load 'context-coloring
;;   '(tern-context-coloring-setup))


;; enable tern-mode and then context-coloring-mode when opening js
;; (add-hook 'js-mode-hook (lambda ()
;;                           (unless (eq major-mode 'json-mode)
;;                             (tern-mode)
;;                             (context-coloring-mode))))

;;------------------------------------------------------------------------------
;; Highlight JavaScript with Tern
;;------------------------------------------------------------------------------
;(require 'tj-mode)
;(add-to-list 'auto-mode-alist '("\\.js\\'" . tj-mode))

;;------------------------------------------------------------------------------
;; for js evaluation in js buffers through indium (formerly jade)
;;------------------------------------------------------------------------------
;; (add-hook 'js2-mode-hook #'jade-interaction-mode)
;; (require 'indium)
;; (add-hook 'js2-mode-hook #'indium-interaction-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'js-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; js-config.el ends here
