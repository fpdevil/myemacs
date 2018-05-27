;;; package --- shell script handling for Emacs
;;;
;;; Commentary:
;;;
;;; Filename   : shell-config.el
;;; description: shell script identify and load for Emacs
;;;
;;;             for shell scripts
;;;
;;; Code:
;;;
;;==============================================================================

(add-to-list 'auto-mode-alist '("\\.bash_profile\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash_history\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'"           . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash\\'"         . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc.local\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'"          . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\'"       . sh-mode))

;;------------------------------------------------------------------------------
;; make a shell script executable automatically on save
;;------------------------------------------------------------------------------
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;------------------------------------------------------------------------------
;; kill the buffer when terminal is exited
;;------------------------------------------------------------------------------
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;;------------------------------------------------------------------------------
;; always use bash
;;------------------------------------------------------------------------------
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;;------------------------------------------------------------------------------
;; utf8
;;------------------------------------------------------------------------------
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)

;;------------------------------------------------------------------------------
;; multi-term
;;------------------------------------------------------------------------------
(defun last-term-buffer (l)
  "Return most recently used term buffer."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
        (car l) (last-term-buffer (cdr l)))))

(defun get-term ()
  "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
  (interactive)
  (let ((b (last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'term-mode major-mode))
        (multi-term)
      (switch-to-buffer b))))

(defun term-send-kill-whole-line ()
  "Kill whole line in term mode."
  (interactive)
  (term-send-raw-string "\C-a")
  (term-send-raw-string "\C-k"))

(defun term-send-kill-line ()
  "Kill line in term mode."
  (interactive)
  (term-send-raw-string "\C-k"))

(setq multi-term-program "/bin/bash")

;;------------------------------------------------------------------------------
;; check `term-bind-key-alist' for key bindings
;;------------------------------------------------------------------------------
(eval-after-load 'multi-term
  '(progn
     (dolist (p '(("C-p" . term-send-up)
                  ("C-n" . term-send-down)
                  ("C-s" . swiper)
                  ("C-r" . term-send-reverse-search-history)
                  ("C-m" . term-send-raw)
                  ("C-k" . term-send-kill-whole-line)
                  ("C-y" . yank)
                  ("C-_" . term-send-raw)
                  ("M-f" . term-send-forward-word)
                  ("M-b" . term-send-backward-word)
                  ("M-K" . term-send-kill-line)
                  ("M-p" . previous-line)
                  ("M-n" . next-line)
                  ("M-y" . yank-pop)
                  ("M-." . term-send-raw-meta)))
       (setq term-bind-key-alist (delq (assoc (car p) term-bind-key-alist) term-bind-key-alist))
       (add-to-list 'term-bind-key-alist p))))

;;------------------------------------------------------------------------------
;; shell completion support
;;------------------------------------------------------------------------------
(defvar ac-source-eshell-pcomplete
  '((candidates . ac-eshell-pcomplete)))

;;------------------------------------------------------------------------------
;; extra information and color for your eshell prompt and command not found
;;------------------------------------------------------------------------------
(use-package eshell-prompt-extras
  :ensure t
  :config
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(use-package eshell-did-you-mean
  :disabled t
  :ensure t
  :defer t
  :init
  (autoload 'eshell-did-you-mean-setup "eshell-did-you-mean")
  (with-eval-after-load 'eshell
    (eshell-did-you-mean-setup)))


(provide 'shell-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; shell-config.el ends here
