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
;;;===========================================================================
(require 'cl)
;;; Code:

(defun get-word-at-point ()
  "Get the current word under the cursor
this function will then move the cursor to the beginning of the word."
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


;; format the Erlang code
(defun format-erl ()
  "Format an Erlang file specified as argument."
  (interactive)
  (message "formatting the code")
  (untabify (point-min) (point-max))
  (condition-case str
    (erlang-indent-current-buffer)
    ('error (message "%s" (error-message-string str))))
  (save-buffer 0))

(provide 'erlang-helper-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; erlang-helper-config ends here.
