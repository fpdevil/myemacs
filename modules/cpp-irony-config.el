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
(require 'company-c-headers)      ;; company backend for irony c-headers
(require 'irony-eldoc)                  ;; eldpc support for irony
(require 'flycheck-irony)               ;; flycheck checker for the C, C++ and Objective-C

;;------------------------------------------------------------------------------
;; set LD_LIBRARY_PATH (for linux) / DYLD_LIBRARY_PATH (for mac)
;;------------------------------------------------------------------------------
;;(setenv "LD_LIBRARY_PATH" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/")
(setenv "LC_CTYPE" "UTF-8")
(setenv "DYLD_LIBRARY_PATH"
        (concat "/opt/software/clang+llvm-7.0.0-x86_64-apple-darwin/lib/"
                ":"
                "/usr/local/opt/opencv3/lib/"))

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

;; irony custom locations for binary, prefix and source
(setq irony-cmake-executable (executable-find "cmake"))
(setq irony-server-install-prefix (concat user-emacs-directory "irony/"))
(setq irony-user-dir (concat user-emacs-directory "irony/"))
(setq irony-cdb-compilation-databases '(irony-cdb-clang-complete
                                        irony-cdb-libclang
                                        ))
;; irony clang additional options
;;(setq irony-additional-clang-options '("-std=c++14"))
(setq irony-additional-clang-options '("-DDEBUG -std=c++14 -Wall"))
;; optional System paths for irony
; (setq irony--compile-options
;   '("-std=c++11"
;     "-stdlib=libc++"))


(after "company"
  ;; A function to add path to company-c-headers
  (defun company-c-headers-includes ()
    ;; You just need to modified the path inside the quote to your header files path
    (setq gcommand "echo | g++ -v -x c++ -E - 2>&1 |
                    grep -A 20 starts |
                    grep include |
                    grep -v search")
    (setq company-c-headers-path-system (mapcar
                                         (lambda (item)
                                           item)
                                         (split-string (shell-command-to-string gcommand)))))

  ;; Now call this function so it add your path to company-c-header-path-system
  (company-c-headers-includes)

  ;; set the company backends as needed by irony
  ; (custom-set-variables
  ;  '(company-backends (quote (company-irony
  ;                             company-c-headers
  ;                             company-irony-c-headers
  ;                             ))))
  ;;(setq-local company-backends
  ;;  '(company-irony company-c-headers company-irony-c-headers))

  (add-hook 'c++-mode-hook
            (lambda () (company-config/push-company-backends-locally 'company-irony)))
  (add-hook 'c++-mode-hook
            (lambda () (company-config/push-company-backends-locally 'company-c-headers)))
   (add-hook 'c++-mode-hook
             (lambda () (company-config/push-company-backends-locally 'company-irony-c-headers)))

  ;; adds CC special commands to `company-begin-commands' in order to trigger
  ;; completion at interesting places, such as after scope operator, std::
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (setq company-backends (delete 'company-semantic company-backends))
  )

;; Irony-mode configuration
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point` and `complete-symbol` bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  "Custom irony mode hook to remap keys."
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))


(dolist (mode '(my-irony-mode-hook
                irony-cdb-autosetup-compile-options))
  (add-hook 'irony-mode-hook mode))

;;------------------------------------------------------------------------------
;;** [eldoc] - documentation for the cpp items
;;------------------------------------------------------------------------------
(add-hook 'irony-mode-hook #'irony-eldoc)


;;------------------------------------------------------------------------------
;;** [flycheck] - realtime syntax checking
;;------------------------------------------------------------------------------
(after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))


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


;;------------------------------------------------------------------------------
;; [auto-complete] - completion mode
;;------------------------------------------------------------------------------
; (after 'auto-complete
;   (defun my:ac-cc-mode-setup ()
;     "AutoComplete CC Mode."
;     (setq ac-sources
;           (append '(ac-source-clang ac-source-yasnippet)
;                   ac-sources))
;     (define-key irony-mode-map (kbd "M-RET") 'ac-complete-irony-async))
;   ;; (add-hook 'c-mode-common-hook 'my:ac-cc-mode-setup)
;   (add-hook 'irony-mode-hook 'my:ac-cc-mode-setup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'cpp-irony-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; ccpp-irony-config.el ends here
