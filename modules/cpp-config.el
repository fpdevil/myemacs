;;; package --- c/c++ configuration settings cpp-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : cpp-config.el
;;; Description: C/C++ IDE Support using the company and irony modes
;;;
;;; elisp code for customizing the C/C++
;;; reference http://nilsdeppe.com/posts/emacs-c++-ide
;;===========================================================================
(require 'irony)
(require 'company)
(require 'company-irony-c-headers)      ;; company backend for irony c-headers
(require 'irony-eldoc)                  ;; eldpc support for irony
(require 'flycheck-irony)               ;; flycheck checker for the C, C++ and Objective-C languages
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup irony modes for c/c++                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  "Custom irony mode hook to remap keys."
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))


(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook 'irony-eldoc)

;;
; Load with `irony-mode` as a grouped backend
;;
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

;; company-irony setup, c-header completions
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;;
; flycheck for irony mode
;;
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(provide 'cpp-config)

;;; cpp-config.el ends here
