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
;;; compilation database generation
;;; $ cd /path/to/project/root
;;; $ cmake . -DCMAKE_EXPORT_COMPILE_COMMANDS=1
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

(use-package find-file
  :defer t
  :init (setq-default ff-always-in-other-window t))


(defun sarcasm-set-c++-cc-style ()
  "Personalized cc-style for c++ mode."
  (c-set-offset 'innamespace 0))

(add-hook 'c++-mode-hook #'sarcasm-set-c++-cc-style)

(define-abbrev-table 'c-mode-abbrev-table
  '(("ASSERT" "#include <assert.h>")
    ("ERRNO"  "#include <errno.h>")
    ("IO" "#include <stdio.h>")
    ("STD"  "#include <stdlib.h>")
    ("STR"  "#include <string.h>")
    ("UNI"  "#include <unistd.h>")))

(define-abbrev-table 'c++-mode-abbrev-table
  '(("ALGO"   "#include <algorithm>")
    ("ASSERT"   "#include <cassert>")
    ("BIT"    "#include <bitset>")
    ("CSTD"   "#include <cstdlib>")
    ("CIO"    "#include <cstdio>")
    ("CMATH"    "#include <cmath>")
    ("DEQUE"    "#include <deque>")
    ("IO"   "#include <iostream>")
    ("EXCEPTION"  "#include <exception>")
    ("LIST"   "#include <list>")
    ("MAP"    "#include <map>")
    ("MANIP"    "#include <iomanip>")
    ("QUEUE"    "#include <queue>")
    ("SET"    "#include <set>")
    ("SSTR"   "#include <sstream>")
    ("STACK"    "#include <stack>")
    ("STDEXCEPT"  "#include <stdexcep>")
    ("STR"    "#include <string>")
    ("UNI"    "#include <unistd.h>")
    ("VECTOR"   "#include <vector>")))



;; irony custom locations for binary, prefix and source
(setq irony-cmake-executable "/opt/software/clang+llvm-6.0.0-x86_64-apple-darwin/bin/clang")
(setq irony-server-install-prefix (concat user-emacs-directory "/irony/"))
(setq irony-user-dir (concat user-emacs-directory "/irony/"))

;; irony clang additional options
(setq irony-additional-clang-options '("-std=c++14"
                                       "-stdlib=libc++"
                                       "-I/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1"
                                       "-I/usr/local/include"
                                       "-I/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/9.1.0/include"
                                       "-I/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
                                       "-I/usr/include"))


(defun err-irony-cdb-not-found (command &rest args)
  (when (eq command 'get-compile-options)
    (message "Irony: compile options not found!")
    nil))

(setq-default irony-cdb-compilation-databases '(irony-cdb-clang-complete
                                                irony-cdb-libclang
                                                err-irony-cdb-not-found))

(setq irony-lang-compile-option-alist
      (quote ((c++-mode . "c++ -std=c++14 -lstdc++")
              (c-mode . "c")
              (objc-mode . "objective-c"))))

;; optional System paths for irony
(setq irony--compile-options
  '("-std=c++14"
    "-stdlib=libc++"))

;; replace the `completion-at-point` and `complete-symbol` bindings in
;; irony-mode's buffers by irony-mode's function
(defun my:irony-mode-hook ()
  "Custom irony mode hook to remap keys."
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))


;; add hooks for the irony-mode
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
;; irony-mode hook that is called when irony is triggered
(add-hook 'irony-mode-hook 'my:irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;;------------------------------------------------------------------------------
;; auto complete mode
;;------------------------------------------------------------------------------
; (defun my-ac-irony-setup ()
;   "AutoComplete CC Mode."
;   (add-to-list 'ac-sources 'ac-source-irony)
;   ; (setq ac-sources
;   ;       (append '(ac-source-irony ac-source-clang ac-source-yasnippet)
;   ;               ac-sources))
;   (define-key irony-mode-map (kbd "M-RET") 'ac-complete-irony-async))
; (add-hook 'irony-mode-hook 'my-ac-irony-setup)


;;------------------------------------------------------------------------------
;; company-irony setup, c-header completions
;;------------------------------------------------------------------------------
(defun company-semantic-setup ()
  "Configure company-backends for company-semantic and company-yasnippet."
  (delete 'company-irony company-backends)
  (push '(company-semantic :with company-yasnippet) company-backends))

;; irony-c-headers
(defun company-irony-c-headers-setup ()
  "Configure company-backends for company-irony-c-headers."
  (add-to-list 'company-backends '(company-irony-c-headers company-irony)))

(after 'company
  ;; adds CC special commands to `company-begin-commands' in order to trigger
  ;; completion at interesting places, such as after scope operator, std::
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (setq-local company-minimum-prefix-length 1)
  (setq company-irony-ignore-case t)
  ;; Remove company-semantic because it has higher precedance than company-clang
  ;; Using RTags completion is also faster than semantic, it seems. Semantic
  ;; also provides a bunch of technically irrelevant completions sometimes.
  ;; All in all, RTags just seems to do a better job.
  (setq-local company-backends (delete 'company-semantic company-backends))
  (setq-local company-backends (delete 'company-clang company-backends))
  (push 'company-irony company-backends)
 '(add-to-list
    'company-backends '(
                        company-irony
                        company-irony-c-headers
                        ;;company-c-headers
                        )))

(defun my-ac-hook ()
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete))
(add-hook 'c++-mode-hook 'my-ac-hook)


;;------------------------------------------------------------------------------
;; Eldoc shows argument list of the function you are currently writing in the
;; echo area. irony-eldoc brings support for C and C++.
;;------------------------------------------------------------------------------
(add-hook 'irony-mode-hook #'irony-eldoc)

;;------------------------------------------------------------------------------
;; flycheck-mode
;;------------------------------------------------------------------------------
(after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))


(when (or (eq major-mode 'c-mode) ; Prevent from being loaded by c derived mode
          (eq major-mode 'c++-mode)
          (eq major-mode 'cuda-mode))
  (ignore-errors
    (irony-mode 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'cpp-irony-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; cpp-irony-config.el ends here
