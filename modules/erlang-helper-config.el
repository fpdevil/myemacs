;;; package  --- erlang-helper-config.el
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : erlang-helper-config.el
;;; Description: Emacs configuration for html development support
;;;
;;; elisp code for handling the keyword pairing in Erlang mode, specifically
;;; for *if*, *end*, *fun*, *case*, *begin*, *receive*
;;;
;;; Code:
;;;

(defun init-fun-hide ()
  (setq hs-special-modes-alist
        (cons '(erlang-mode
                "^\\([a-z][a-zA-Z0-9_]*\\|'[^\n']*[^\\]'\\)\\s *(" nil "%"
                erlang-end-of-clause) hs-special-modes-alist))
  (local-set-key [?\M-s] 'hs-toggle-hiding)
  (local-set-key [?\M-h] 'hs-hide-all)
  (local-set-key [?\M-u] 'hs-show-all))
(add-hook 'erlang-mode-hook 'init-fun-hide)

;;-------------------------------------------------------------------
;;** helper for formatting the erlang records
;;-------------------------------------------------------------------
(defun align-erlang-record ()
  "Formatting the erlang record data structure."
  (interactive)
  (let ((from (line-beginning-position)))
    (goto-char from)
    (search-forward "-record" )
    (search-forward "{")
    (goto-char (- (point) 1))
    (ignore-errors (er/expand-region 1))
    (my-align-region-by "=")
    (goto-char from)
    (search-forward "-record" )
    (search-forward "{")
    (goto-char (- (point) 1))
    (ignore-errors (er/expand-region 1))
    (my-align-region-by "::")))


;;-------------------------------------------------------------------
;;** Font Locking, Prettify symbols and matching parentheses highlight
;;-------------------------------------------------------------------
(global-font-lock-mode t)

(add-hook 'erlang-mode-hook 'erlang-font-lock-level-4)
(defun erlang-prettify-symbols ()
  "Prettify common erlang symbols."
  (setq prettify-symbols-alist
        '(
          ("fun"       . ?ƒ)
          ("->"        . ?➔)
          ("<-"        . ?∈)
          ("=/="       . ?≠)
          ("=:="       . ?≡)
          ("=="        . ?≈)
          ("and"       . ?∧)
          ("or"        . ?∨)
          (">="        . ?≥)
          ("=<"        . ?≤)
          ("lists:sum" . ?∑)
          )))

;; Set face of exported functions
(when (boundp 'erlang-font-lock-exported-function-name-face)
  (set-face-attribute 'erlang-font-lock-exported-function-name-face nil
                      :underline t))

(defun my-delayed-prettify ()
  "Mode is guaranteed to run after the style hooks."
  (run-with-idle-timer 0 nil (lambda () (prettify-symbols-mode 1))))

(add-hook 'erlang-mode-hook 'my-delayed-prettify)
(add-hook 'erlang-mode-hook 'erlang-prettify-symbols)


(defun get-word-at-point ()
  "Get the current word under the cursor this function will then move the cursor to the beginning of the word."
  (let (tail-point)
    (skip-chars-forward "-_A-Za-z0-9")
    (setq tail-point (point))
    (skip-chars-backward "-_A-Za-z0-9")
    (buffer-substring-no-properties (point) tail-point)))

(defun erl-pair-keyword-valid-p ()
  "Find a valid value for the pair."
  (let ((line-head-point (line-beginning-position)))
    (if (and
         (= 0 (% (count-matches "\"" line-head-point (point)) 2))
         (= 0 (% (count-matches "'" line-head-point (point)) 2))
         (= 0 (count-matches "%" line-head-point (point))))
        t
      nil)))

(defun erl-pair-construct-stack (value old-stack)
  "Construct a pair with VALUE and OLD-STACK."
  (if (= 0 (+ value (car old-stack)))
      (cdr old-stack)
    (cons value old-stack)))

(defun erl-pair-find (direction stack origin-point)
  "Get the pair for DIRECTION STACK ORIGIN-POINT."
  (catch 'ok
    (while t
      (condition-case nil
          (progn
            (funcall direction "\\(^\\|[\s\t\\[(=>]\\)\\(case\\|if\\|begin\\|receive\\|fun[\s\t\n]*(\.*\\|end\\)\\($\\|[\s\t,;.]\\)")
            (goto-char (match-beginning 2))
            (setq stack
                  (if (not (erl-pair-keyword-valid-p))
                      stack
                    (if (looking-at "end")
                        (erl-pair-construct-stack -1 stack)
                      (erl-pair-construct-stack 1 stack))))
            (if stack
                (forward-char)
              (throw 'ok t)))
        (error (progn
                 (message "Wrong format")
                 (goto-char origin-point)
                 (throw 'ok t)))))))

(defun erl-find-pair ()
  "Find a pair for if, case, begin in the Erlang mode."
  (interactive)
  (when (eq major-mode 'erlang-mode)
    (let ((keywords '("case" "if" "begin" "receive")))
      (when (erl-pair-keyword-valid-p)
        (if (or (member (get-word-at-point) keywords)
                (looking-at "fun[\s\t\n]*(")) ; 'fun' is an except
            (progn
              (forward-char)
              (erl-find-pair 'search-forward-regexp '(1) (point)))
          (if (equal (get-word-at-point) "end")
              (progn
                ;; (backward-char)
                (erl-find-pair 'search-backward-regexp '(-1) (point)))))))))


;;-------------------------------------------------------------------
;;** [Code Format] - format the Erlang code
;;-------------------------------------------------------------------
(defun format-erl ()
  "Format an Erlang file specified as argument."
  (interactive)
  (message "formatting the code")
  (untabify (point-min) (point-max))
  (condition-case str
      (erlang-indent-current-buffer)
    ('error (message "%s" (error-message-string str))))
  (save-buffer 0))


;;-------------------------------------------------------------------
;;** electric commands
;;-------------------------------------------------------------------
(defun erl/electric-mode-hook ()
  "Handle the electric commands."
  (setq erlang-electric-commands
	;; Insert a comma character and possibly a new indented line.
	'(erlang-electric-comma
	  ;; Insert a semicolon character and possibly a prototype for the next line.
	  erlang-electric-semicolon
	  ;; Insert a '>'-sign and possible a new indented line.
	  erlang-electric-gt
	  ))
  (setq erlang-electric-newline-inhibit-list
	'(erlang-electric-gt))
  (setq erlang-electric-newline-inhibit t))
(add-hook 'erlang-mode-hook 'erl/electric-mode-hook)

;;-------------------------------------------------------------------
;;** new file declarations
;;-------------------------------------------------------------------
(defun erl-file-header ()
  "Insert a custom edoc header at the top."
  (interactive)
  (save-excursion
    (when (re-search-forward "^\\s *-spec\\s +\\([a-zA-Z0-9_]+\\)\\s *(\\(\\(.\\|\n\\)*?\\))\\s *->[ \t\n]*\\(.+?\\)\\." nil t)
      (let* ((beg (match-beginning 0))
             (funcname (match-string-no-properties 1))
             (arg-string (match-string-no-properties 2))
             (retval (match-string-no-properties 4))
             (args (split-string arg-string "[ \t\n,]" t)))
        (when (re-search-forward (concat "^\\s *" funcname "\\s *(\\(\\(.\\|\n\\)*?\\))\\s *->") nil t)
          (let ((arg-types (split-string (match-string-no-properties 1) "[ \t\n,]" t)))
            (goto-char beg)
            (insert "%%-----------------------------------------------------------------------------\n")
            (insert "%% @doc\n")
            (insert "%% Your description goes here\n")
            (insert "%% @spec " funcname "(")
            (dolist (arg args)
              (if (string-match "::" arg) (insert arg) (insert (car arg-types) "::" arg))
              (setq arg-types (cdr arg-types))
              (when arg-types
                (insert ", ")))
            (insert ") ->\n")
            (insert "%%       " retval "\n")
            (insert "%% @end\n")
            (insert "%%-----------------------------------------------------------------------------\n")))))))

;;** erlang skels options
(eval-after-load "erlang-skels"
  (progn
    (setq erlang-skel-author "Sampath Singamsetty")
    (setq erlang-skel-mail-address "Singamsetty.Sampath@gmail.com")))


;; Open documentation for erlang modules in a web browser
(defvar aqua/erlang-doc-root
  erlang-root-dir
  "Path to the local Erlang OTP documentation (HTTP format).")
(defvar browse-erlang-doc-history nil)
(defvar erlang-doc-root-dir aqua/erlang-doc-root)
(defun browse-erlang-doc (module)
  "Open documentation for erlang module MODULE in a web browser."
  (interactive
   (let* ((files
           (append
            (file-expand-wildcards
             (concat erlang-doc-root-dir
                     "/lib/*/doc/html/*.html"))))
          (completion-table
           (mapcar
            (lambda (file)
              (cons (file-name-sans-extension
                     (file-name-nondirectory file))
                    file))
            files))
          (module-name (completing-read "Search: "
                                        completion-table
                                        nil t nil
                                        'browse-erlang-doc-history)))
     (list (cdr (assoc module-name completion-table)))))
  (browse-url-of-file module))
(local-set-key (kbd "C-e d") 'browse-erlang-doc)

(provide 'erlang-helper-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; erlang-helper-config.el ends here
