;;; package  --- flyspell-config.el
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : flyspell-config.el
;;; Description: Emacs enable spell checking for comments & text
;;;              spell checking on the fly.
;;;
;;; Code:
;;;
;;;=============================================================================
(lazy-init
  (require 'flyspell)                                           ; flyspell mode

  ;;------------------------------------------------------------------------------
  ;; flyspell checking for comments and text mode
  ;;------------------------------------------------------------------------------
  ;; do a spell check on the comments for c & c++
  (add-hook 'c++-mode-hook 'flyspell-prog-mode)
  (add-hook 'c-mode-common-hook 'flyspell-prog-mode)

  ;; enable flyspell in text mode(s)
  (if (fboundp 'prog-mode)
      (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (dolist (hook '(lisp-mode-hook
                    emacs-lisp-mode-hook
                    clojure-mode-hook
                    python-mode-hook
                    shell-mode-hook
                    css-mode-hook
                    js-mode-hook
                    javascript-mode-hook
                    haskell-mode-hook
                    go-mode-hook
                    erlang-mode-hook
                    nxml-mode-hook))
      (add-hook hook 'flyspell-prog-mode)))

  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook (hook (lambda () (flyspell-mode -1)))))

  ;;------------------------------------------------------------------------------
  ;; improve performance by not printing messages for every word
  ;;------------------------------------------------------------------------------
  (setq flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil
        flyspell-use-meta-tab nil)

  ;; Don't consider that a word repeated twice is an error
  (setq flyspell-mark-duplications-flag nil)
  ;; Lower (for performance reasons) the maximum distance for finding
  ;; duplicates of unrecognized words (default: 400000)
  (setq flyspell-duplicate-distance 12000)
  ;; Dash character (`-') is considered as a word delimiter
  (setq flyspell-consider-dash-as-word-delimiter-flag t)

  ;; load flyspell-lazy
  (eval-after-load 'flyspell
    '(progn
       (require 'flyspell-lazy)
       (flyspell-lazy-mode 1)))

  ;;------------------------------------------------------------------------------
  ;;;           find aspell load
  ;;------------------------------------------------------------------------------
  (setq ispell-program-name (executable-find "aspell")
        ispell-dictionary "american" ; better for aspell
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")
        ispell-list-command "--list")

  (add-to-list 'ispell-local-dictionary-alist '(nil
                                                "[[:alpha:]]"
                                                "[^[:alpha:]]"
                                                "['‘’]"
                                                t
                                                ("-d" "en_US")
                                                nil
                                                utf-8))

  ;; Use helm with flyspell
  (define-key flyspell-mode-map (kbd "<f8>") 'helm-flyspell-correct)

  ;; flyspell line styles
  (custom-set-faces
   '(flyspell-duplicate ((t (:underline (:color "Blue" :style wave)))))
   '(flyspell-incorrect ((t (:underline (:color "Purple" :style wave))))))

  ;;------------------------------------------------------------------------------
  ;; flyspell setup for js2-mode
  ;;------------------------------------------------------------------------------
  (defun js-flyspell-verify ()
    (let* ((f (get-text-property (- (point) 1) 'face)))
      ;; *whitelist*
      ;; only words with following font face will be checked
      (memq f '(js2-function-call
                js2-function-param
                js2-object-property
                font-lock-variable-name-face
                font-lock-string-face
                font-lock-function-name-face
                font-lock-builtin-face
                rjsx-tag
                rjsx-attr))))
  (put 'js2-mode 'flyspell-mode-predicate 'js-flyspell-verify)
  (put 'rjsx-mode 'flyspell-mode-predicate 'js-flyspell-verify)

  ;;------------------------------------------------------------------------------
  ;; company ispell integration
  ;;------------------------------------------------------------------------------
  (defun company-text-mode-hook ()
    "Company for `text-mode'"
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends 'company-ispell)
    (setq company-ispell-dictionary (file-truename "~/.emacs.d/private/english-words.txt")))
  (add-hook 'text-mode-hook 'company-text-mode-hook)

  (defun toggle-company-ispell ()
    "M-x toggle the company iSpell."
    (interactive)
    (cond
     ((memq 'company-ispell company-backends)
      (setq company-backends (delete 'company-ispell company-backends))
      (message "company-ispell disabled"))
     (t
      (add-to-list 'company-backends 'company-ispell)
      (message "company-ispell enabled!"))))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'flyspell-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; no-byte-compile t
;; End:

;;; flyspell-config.el ends here
