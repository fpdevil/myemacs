;;; package  --- js-config.el
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : javascript-config.el
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

(require 'js2-mode)                 ;; js2 javascript mode
(require 'js2-highlight-vars)       ;; highlight occurrences of vars
(require 'js-doc)                   ;; insert JsDoc style comment easily
(require 'jsfmt)                    ;; formatting with jsfmt
(require 'json-mode)                ;; for json
(require 'json-navigator)           ;; view json docs (M-x json-navigator)

;; make return key also do indent, for current buffer only
(electric-indent-local-mode 1)

;;------------------------------------------------------------------------------
;;** [file types] - file modes association and auto lists
;;------------------------------------------------------------------------------
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'"         . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js$"           . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'"        . js2-mode))
(add-to-list 'auto-mode-alist '("\\.eslintrc.*$"   . json-mode))
(add-to-list 'auto-mode-alist '("\\.babelrc$"      . json-mode))
(add-to-list 'auto-mode-alist '("\\.jsbeautifyrc$" . json-mode))
(add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))

;;------------------------------------------------------------------------------
;;** [js2 and json modes]
;;** https://truongtx.me/2014/02/23/set-up-javascript-development-environment-in-emacs
;;------------------------------------------------------------------------------
(add-hook 'js-mode-hook 'js2-minor-mode)
;; for better imenu support, enable the extras
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

;;------------------------------------------------------------------------------
;;** [highlight variables] - js2 variable highlighting where applicable
;;------------------------------------------------------------------------------
(after "js2-highlight-vars-autoloads"
  '(add-hook 'js2-mode-hook (lambda () (js2-highlight-vars-mode))))

;;------------------------------------------------------------------------------
;;** [js2 customizations] - syntax specific and key binding settings
;;------------------------------------------------------------------------------
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
;;** [refactoring] - javascript js2 refactoring
;; javascript code refactoring - https://github.com/magnars/js2-refactor.el
;;------------------------------------------------------------------------------
(after [js2-mode-autoloads js2-refactor]
  ;; kbd prefix as C-c C-m (extract function with `C-c C-m ef`)
  (js2r-add-keybindings-with-prefix "C-c C-m"))

;;------------------------------------------------------------------------------
;;** [js-mode indentation levels] - Change some defaults: customize them to override
;;------------------------------------------------------------------------------
(defconst preferred-javascript-indent-level 4)
(setq-local js-indent-level preferred-javascript-indent-level)
(setq-local js2-basic-offset preferred-javascript-indent-level)
(setq-local javascipt-indent-level preferred-javascript-indent-level)

;;------------------------------------------------------------------------------
;;** [json-mode] - indentation settings
;;------------------------------------------------------------------------------
(add-hook 'json-mode-hook
          (lambda ()
            (setq js-indent-level preferred-javascript-indent-level)))

;;------------------------------------------------------------------------------
;; [Tern JS] - javascript auto-completion
;;------------------------------------------------------------------------------
(defun ac-load-tern ()
  (eval-after-load 'tern
    '(progn
       (require 'auto-complete-config)
       (ac-config-default)
       (require 'tern-auto-complete)
       (tern-ac-setup)))
  ;;; Start tern-mode automatically when starting js mode
  (add-hook 'js-mode-hook (lambda () (tern-mode t))))

;; (add-hook 'after-init-hook 'ac-load-tern)

(defun company-load-tern ()
  (use-package company-tern
    :commands company-tern
    :hook (tern-mode . (lambda ()
      (setq-local company-backends '(company-tern company-capf)))))
  ;;; Start tern-mode automatically when starting js mode
  (add-hook 'js-mode-hook (lambda () (tern-mode t))))

(add-hook 'after-init-hook 'company-load-tern)

;;------------------------------------------------------------------------------
;;** Force restart of tern in new projects (M-x delete-tern-process)
;;** If we have just added the .tern-project file or edit the file but Tern
;;** does not auto reload, we need to manually kill Tern server
;;------------------------------------------------------------------------------
(defun delete-tern-process ()
  "If Tern based auto-refresh is not happening disable Tern."
  (interactive)
  (delete-process "Tern"))

;;------------------------------------------------------------------------------
;;** code beautification through web-beautify and js-beautify npm package
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
;;** [ skewer ] - live loading with skewer (for front-end)
;;** [ livid ] - Live browser eval of JavaScript every time a buffer changes
;;------------------------------------------------------------------------------
;; (after "skewer-mode"
;;   (add-hook 'js2-mode-hook 'skewer-mode)
;;   (add-hook 'css-mode-hook 'skewer-css-mode)
;;   (add-hook 'html-mode-hook 'skewer-html-mode))

(use-package skewer-mode
  :about live browser JavaScript, CSS, and HTML interaction
  :homepage https://github.com/skeeto/skewer-mode
  :disabled t
  :ensure t
  :defer t
  :init
  (add-hook 'js2-mode-hook #'skewer-mode)
  (add-hook 'css-mode-hook #'skewer-css-mode)
  (add-hook 'html-mode-hook #'skewer-html-mode))

(require 'livid-mode)

;;------------------------------------------------------------------------------
;; [jsfmt] - For formatting, searching, and rewriting javascript
;; prerequisite npm install -g jsfmt
;;------------------------------------------------------------------------------
(add-hook 'before-save-hook 'jsfmt-before-save)

;;------------------------------------------------------------------------------
;; [indentation] - additional setup for indentation
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
;; [js-doc] - JsDoc style documentation and comments
;; 1. insert function document by pressing Ctrl + c, i
;; 2. insert @tag easily by pressing @ in the JsDoc style comment
;;------------------------------------------------------------------------------
(setq js-doc-mail-address "Sampath.Singamsetty"
      js-doc-author (format "Sampath Singamsetty <%s>" js-doc-mail-address)
      js-doc-url "https://github.com/fpdevil"
      js-doc-license "MIT License")

(add-hook 'js2-mode-hook
          #'(lambda ()
              (define-key js2-mode-map "\C-c i" 'js-doc-insert-function-doc)
              (define-key js2-mode-map "@" 'js-doc-insert-tag)))

;;------------------------------------------------------------------------------
;; [FlyCheck jsxhint] - syntax checking and linting
;; first install checker using package manager - npm install -g jsxhint
;;------------------------------------------------------------------------------
(after "flycheck"
  '(progn
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
;; [eslint] - use local eslint from node_modules before global
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

;; Manual jshint invocation
;; Configure jshint for JS style checking.
;;   - Install: $ npm install -g jshint
;;   - Usage: Hit C-c C-u within any emacs buffer visiting a .js file
(setq jshint-cli "jshint --show-non-errors ")
(setq compilation-error-regexp-alist-alist
      (cons '(jshint-cli "^\\([a-zA-Z\.0-9_/-]+\\): line \\([0-9]+\\), col \\([0-9]+\\)"
                         1 ;; file
                         2 ;; line
                         3 ;; column
                         )
            compilation-error-regexp-alist-alist))
(setq compilation-error-regexp-alist
      (cons 'jshint-cli compilation-error-regexp-alist))

;;------------------------------------------------------------------------------
;; [FlyCheck jslint] - (npm install jslinter -g)
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
;; [ FlyCheck ] syntax checker
;;------------------------------------------------------------------------------
(add-hook 'js-mode-hook (lambda () (flycheck-mode t)))

;;------------------------------------------------------------------------------
;;; [ import-js ] -- A tool to simplify importing of JS modules.
;;------------------------------------------------------------------------------
(require-package 'import-js)
(require 'import-js)

;;------------------------------------------------------------------------------
;; [pretty symbols] - prettify symbols in javascript
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
;; [REPL Interaction] - send code to a running node repl
;;------------------------------------------------------------------------------
(require 'js-comint)

(provide 'javascript-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; javascript-config.el ends here
