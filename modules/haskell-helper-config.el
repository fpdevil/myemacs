;;; package --- helper configuration settings for Haskell
;;;
;;; Commentary:
;;;
;;; Filename   : haskell-helper-config.el
;;; Description: Haskell Auto Insert doc strings and any other...
;;;              Haskell syntax checking with flymake & flycheck
;;;
;;; elisp code for haskell helpers
;;===========================================================================

;;;
;;; Code:
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake handler for checking Haskell source code with hlint.            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'haskell-mode-hook 'flymake-hlint-load)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flcheck handler for checking Haskell source code with hlint.            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load 'flycheck
  '(progn
     (flycheck-mode)
     (setq flycheck-haskell-hlint-executable
           (concat (getenv "HOME") "/Library/Haskell/bin/hlint"))
     ;; (setq flycheck-check-syntax-automatically '(save))
     (add-hook 'haskell-mode-hook
               '(lambda ()
                  (setq flycheck-checker 'haskell-hlint)
                  (setq flycheck-disabled-checkers '(haskell-ghc))
                  ;(flycheck-mode 1)
                  ))
     (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
     (add-hook 'haskell-mode-hook 'flycheck-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-function-mode for Haskell buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(which-function-mode)
(eval-after-load "which-func"
  '(add-to-list 'which-func-modes 'haskell-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; intero (not used, so commented)                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (defun haskell-process-cabal-build-and-restart ()
;   "Build and restart the Cabal project."
;   (interactive)
;   (intero-devel-reload))
;
; (add-hook 'haskell-mode-hook 'intero-mode)
; ;; key map
; (define-key intero-mode-map (kbd "C-`") 'flycheck-list-errors)
; (define-key intero-mode-map [f12] 'intero-devel-reload)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DOC Strings                                                                ;;
;; auto insert haskell module header when a new file is created               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'skeleton)
(require 'autoinsert)

;; A Haskell module definition template
(define-skeleton haskell-module-skeleton
  "Haskell source module header."
  "Brief description (can be left blank for defaults):"
  "{- \|\n"
  "   Module      : " (setq v1 (or (haskell-guess-module-name) "Main")) "\n"
  "   Description : " str | (concat "The \\\"" v1 "\\\" module") "\n"
  "   Copyright   : " (haskell-cabal-guess-setting "copyright") | (concat "(c) " user-full-name) "\n"
  "   License     : " (haskell-cabal-guess-setting "license") | "BSD-style (see the file LICENSE)" "\n"
  "   Maintainer  : " (haskell-cabal-guess-setting "maintainer") | user-mail-address "\n"
  "\n"
  "   " _ "\n"
  "\n"
  " -}\n"
  "module " v1 " where\n\n")

;; uncomment the below line to insert module docstring as defined in haskell-module-skeleton
;; (add-to-list 'auto-insert-alist '("\\.hs\\|.lhs\\'" . haskell-module-skeleton))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert comment doc with C-c C-a
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun haskell-insert-doc ()
  "Insert the documentation syntax."
  (interactive)
  (unless (= (line-beginning-position)
             (line-end-position))
    (shm/backward-paragraph))
  (unless (= (line-beginning-position)
             (line-end-position))
    (save-excursion (insert "\n")))
  (insert "-- | "))

(defun haskell-insert-undefined ()
  "Insert undefined."
  (interactive)
  (if (and (boundp 'structured-haskell-mode)
           structured-haskell-mode)
      (shm-insert-string "undefined")
    (insert "undefined")))

(define-key haskell-mode-map (kbd "C-c C-a") 'haskell-insert-doc)
(define-key haskell-mode-map (kbd "C-c C-u") 'haskell-insert-undefined)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a simple document string insertion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun haskell-insert-docstring ()
  "Insert a simple module template for the newly created buffer."
  (interactive)
  (when (and (= (point-min)
                (point-max))
             (buffer-file-name))
    (insert
     "-- | "
     "\n"
     "\n"
     "module "
     )
    (let ((name (haskell-guess-module-name)))
      (if (string= name "")
          (progn (insert "Main")
                 (shm-evaporate (- (point) 5)
                                (point)))
        (insert name)))
    (insert " where"
            "\n"
            "\n")
    (goto-char (point-min))
    (forward-char 4)))

;; in order to use the above to insert doc string, uncomment below
(add-hook 'haskell-mode-hook 'haskell-insert-docstring)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'haskell-helper-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; haskell-helper-config.el ends here
