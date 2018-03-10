;;; package --- c/c++ configuration settings cpp-irony-config
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : cpp-irony-config.el
;;; Description: C/C++ IDE Support using the company and irony modes
;;;
;;; run the below and add into ..clang_complete
;;; echo | clang -x c++ -v -E - 2>&1
;;; | sed -n '/^#include </,/^End/s|^[^/]*\([^ ]*/include[^ ]*\).*$|-I\1|p'
;;;
;;; cpp -v
;;; Code:
;;;;===========================================================================
(require 'irony)                        ;; irony cpp ide plugin
(require 'company-irony-c-headers)      ;; company backend for irony c-headers
(require 'irony-eldoc)                  ;; eldpc support for irony
(require 'flycheck-irony)               ;; flycheck checker for the C, C++ and Objective-C

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        setup irony modes for c/c++                         ;;
;; A C/C++ minor mode for Emacs powered by libclang                           ;;
;; https://github.com/Sarcasm/irony-mode/wiki/Mac-OS-X-issues-and-workaround  ;;
;; refer my own installation log in root irony-install.md                     ;;
;;  ---------------------------- check commands ----------------------------  ;;
;; xcodebuild -find make                                                      ;;
;; xcodebuild -find gcc                                                       ;;
;; xcodebuild -find g++                                                       ;;
;; xcodebuild -find clang                                                     ;;
;; xcodebuild -find clang++                                                   ;;
;;                                                                            ;;
;; echo echo "" | g++ -v -x c++ -E -                                          ;;
;; echo echo "" | gcc -xc -E -v -                                             ;;
;; echo echo "" | gcc -xc++ -E -v -                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (hook '(c++-mode-hook
                c-mode-hook
                objc-mode-hook
                eldoc-mode))
  (add-hook hook 'irony-mode))

;; irony custom locations for binary, prefix and source
(setq irony-cmake-executable (executable-find "cmake"))
(setq irony-server-install-prefix (concat user-emacs-directory "/irony/"))
(setq irony-user-dir (concat user-emacs-directory "/irony/"))

;; irony clang additional options
(setq irony-additional-clang-options '("-I/Library/Developer/CommandLineTools/usr/include/c++/v1"
                                       "-std=c++11"))


;; replace the `completion-at-point` and `complete-symbol` bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  "Custom irony mode hook to remap keys."
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

;; add hooks for the irony-mode
(dolist (mode '(my-irony-mode-hook
                irony-cdb-autosetup-compile-options
                irony-eldoc))
  (add-hook 'irony-mode-hook mode))

(after 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; Integrate Clang Static Analyzer with flycheck for on-the-fly
;; static analysis in Emacs
(with-eval-after-load 'flycheck
  (require-package 'flycheck-clang-analyzer)
  (flycheck-clang-analyzer-setup))

;;------------------------------------------------------------------------------
;; bind TAB key for indent-or-complete (optional)
;;------------------------------------------------------------------------------
(defun irony--check-expansion ()
  "TAB for indent or complete."
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))


(defun irony--indent-or-complete ()
  "Indent or Complete."
  (interactive)
  (cond ((and (not (use-region-p))
           (irony--check-expansion))
          (message "complete")
          (company-complete-common))
    (t (message "indent")
      (call-interactively 'c-indent-line-or-region))))


(defun irony-mode-keys ()
  "Modify keymaps used by `irony-mode'."
  (local-set-key (kbd "TAB") 'irony--indent-or-complete)
  (local-set-key [tab] 'irony--indent-or-complete))
(add-hook 'c-mode-common-hook 'irony-mode-keys)

;; optional System paths for irony
(setq irony--compile-options
  '("-std=c++11"
    "-stdlib=libc++"
    "-I/System/Library/Frameworks/Python.framework/Headers"
    "-isysroot"
    "-I/usr/include/c++/4.2.1"))

;;------------------------------------------------------------------------------
;; company-irony setup, c-header completions
;;------------------------------------------------------------------------------
;; Load with the `irony-mode` as a grouped back-end
(after "company"
  ;; adds CC special commands to `company-begin-commands' in order to trigger
  ;; completion at interesting places, such as after scope operator, std::
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (when (boundp 'company-backends)
    (make-local-variable 'company-backends)
    (setq company-backends (delete 'company-clang company-backends))
    '(add-to-list 'company-backends
      '(
        company-irony-c-headers
        company-irony
        company-clang
        ;; company-rtags
        ))))

;;------------------------------------------------------------------------------
;; auto complete mode
;;------------------------------------------------------------------------------
(after 'auto-complete
  (defun my:ac-cc-mode-setup ()
  "AutoComplete CC Mode."
  (setq ac-sources
        (append '(ac-source-clang ac-source-yasnippet)
                ac-sources)))
  ;; (add-hook 'c-mode-common-hook 'my:ac-cc-mode-setup)
  (add-hook 'irony-mode-hook 'my:ac-cc-mode-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'cpp-irony-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; ccpp-irony-config.el ends here
