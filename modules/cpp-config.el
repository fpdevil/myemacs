;;; package --- c/c++ configuration settings cpp-config.el
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : cpp-config.el
;;; Description: C/C++ IDE Support using the company and irony modes
;;;
;;; elisp code for customizing the C/C++ development on Emacs
;;; reference http://nilsdeppe.com/posts/emacs-c++-ide
;;===========================================================================
(require 'cl)
(require 'cc-mode)                      ;; major mode for c and similar languages
(require 'irony)                        ;; irony cpp ide plugin
(require 'company-c-headers)            ;; company backends for completing C/C++ headers
(require 'company-irony-c-headers)      ;; company backend for irony c-headers
(require 'irony-eldoc)                  ;; eldpc support for irony
(require 'flycheck-irony)               ;; flycheck checker for the C, C++ and Objective-C
(require 'auto-complete-clang)          ;; auto complete source for clang. AC+Clang+Yasnippet
(require 'auto-complete-c-headers)      ;; auto-complete source for C/C++ header files
(require 'google-c-style)               ;; google's c/c++ style for c-mode
(require 'clang-format)                 ;; code formatting

;;;
;;; Code:
;;;

(load-file (concat module-dir "/cpp-helper-config.el"))
(require 'cpp-helper-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OSX System base path for Xcode platform                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar osx-base-path
  "/Applications/Xcode.app/Contents/Developer/Platforms/")

;; making code gnu style
(setq c-default-style "linux"
      c-basic-offset 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup irony modes for c/c++                                              ;;
;; https://github.com/Sarcasm/irony-mode/wiki/Mac-OS-X-issues-and-workaround;;
;; refer my own instalaltion log in root irony-install.md                   ;;
;; ---------------------------- check commands ---------------------------- ;;
;; xcodebuild -find make                                                    ;;
;; xcodebuild -find gcc                                                     ;;
;; xcodebuild -find g++                                                     ;;
;; xcodebuild -find clang                                                   ;;
;; xcodebuild -find clang++                                                 ;;
;;                                                                          ;;
;; echo echo "" | g++ -v -x c++ -E -                                        ;;
;; echo echo "" | gcc -xc -E -v -                                           ;;
;; echo echo "" | gcc -xc++ -E -v -                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (hook '(c++-mode-hook
                c-mode-hook
                objc-mode-hook))
  (add-hook hook 'irony-mode))

;; irony custom locations for binary, prefix and source
(setq irony-cmake-executable "/usr/local/bin/cmake")
(setq irony-server-install-prefix (concat (getenv "HOME") "/.emacs.d/irony/"))
(setq irony-user-dir (concat (getenv "HOME") "/.emacs.d/irony/"))


;; replace the `completion-at-point` and `complete-symbol` bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  "Custom irony mode hook to remap keys."
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

;; add hooks to irony-mode
(dolist (mode '(my-irony-mode-hook
                irony-cdb-autosetup-compile-options
                irony-eldoc))
  (add-hook 'irony-mode-hook mode))

;; company-irony setup, c-header completions
;; adds CC special commands to `company-begin-commands' in order to trigger
;; completion at interesting places, such as after scope operator, std::
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; delete company-semantic because it has higher precedance than company-clang
(setq company-backends (delete 'company-semantic company-backends))

;; Load with the `irony-mode` as a grouped back-end
(eval-after-load 'company
  '(add-to-list (make-local-variable 'company-backends)
    '(company-irony-c-headers
      company-irony
      company-yasnippet
      company-clang
      ;; company-rtags
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; disable and enable company-semantic backend at will                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-disable-semantic ()
  "Disable the company-semantic backends."
  (interactive)
  (setq company-backends  (delete '(company-irony-c-headers
                                    company-irony
                                    company-yasnippet
                                    company-clang
                                    ;; company-rtags
                                    company-semantic)
                            company-backends))
  (add-to-list 'company-backends '(company-irony-c-headers
                                   company-irony
                                   company-yasnippet
                                   ;; company-rtags
                                   company-clang)))

(defun my-enable-semantic ()
  "Enable the company-semantic backends."
  (interactive)
  (setq company-backends (delete '(company-irony-c-headers
                                   company-irony
                                   company-yasnippet
                                   company-clang)
                           company-backends))
  (add-to-list 'company-backends '(company-irony-c-headers
                                   company-irony
                                   company-yasnippet
                                   company-clang)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Format code using clang-format /usr/local/bin/clang-format             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [C-M-tab] 'clang-format-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; using flycheck for irony mode                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bind TAB for indent-or-complete (optional)                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ac-clang-flags to include from echo "" | g++ -v -x c++ -E -            ;;;
;;; add necessary include locations                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ac-clang-flags (append
      (mapcar (lambda (item)(concat "-I" item))
              (split-string
               "
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/8.0.0/include
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
/usr/local/opt/opencv3/include
/usr/local/include
/usr/include
")) ac-clang-flags))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-complete settings                                                 ;;;
;;; define a function which initializes auto-complete-c-headers and then   ;;;
;;; gets called for the relevant c/c++ hooks                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my:ac-c-header-init ()
  "Auto completion using ac."
  (add-to-list 'ac-sources 'ac-source-c-headers)
  ;; execute command `gcc -xc++ -E -v -` to find the header directories
  (add-to-list 'achead:include-directories '"/usr/local/opt/gcc/include/c++/6.3.0")
  (add-to-list 'achead:include-directories '"/usr/include/c++/4.2.1")
  (add-to-list 'achead:include-directories '"/usr/local/opt/opencv3/include")
  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include")
  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/8.0.0/include")
  (add-to-list 'achead:include-directories '"/usr/local/include")
  (add-to-list 'achead:include-directories '"/usr/include")
  )

; now let's call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; set LD_LIBRARY_PATH (for linux) / DYLD_LIBRARY_PATH (for mac)          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(setenv "LD_LIBRARY_PATH" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/")
(setenv "LC_CTYPE" "UTF-8")
(setenv "DYLD_LIBRARY_PATH"
  (concat "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/"
          ":"
          "/usr/local/opt/opencv3/lib/"))


(defun my:ac-cc-mode-setup ()
  "AutoComplete CC Mode."
  (setq ac-sources
        (append '(ac-source-clang ac-source-yasnippet)
                ac-sources)))
(add-hook 'c-mode-common-hook 'my:ac-cc-mode-setup)

;;--------------------------------------------------------------------------;;
;; for company completion                                                   ;;
;; c++ header completion for standard libraries                             ;;
;;--------------------------------------------------------------------------;;
(add-to-list 'company-c-headers-path-system "/usr/local/opt/opencv3/include")
(defun my:company-c-headers-init()
  "Build the c headers for adding."
  ;;(setq company-idle-delay nil)
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (mapcar (lambda (item)
            (add-to-list
             'company-c-headers-path-system item))
          '("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1"
            "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/8.0.0/include"
            "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
            "/usr/local/opt/gcc/include/c++/6.3.0"
            "/usr/include/c++/4.2.1"
            "/usr/local/opt/opencv3/include"
            "/usr/local/include"
            "/usr/include"
           )))

;; company-clang system paths
(setq company-clang-executable
  "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang++"
  company-clang-arguments
  '("-std=c++11"
     "-stdlib=libc++"
     "-I/System/Library/Frameworks/Python.framework/Headers"
     "-isysroot"
     "-I/usr/include/c++/4.2.1"
     ))


;; now let's call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:company-c-headers-init)
(add-hook 'c-mode-hook 'my:company-c-headers-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my:flymake-google-init()
  (require 'flymake-google-cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command "/usr/local/bin/cpplint"))
  (flymake-google-cpplint-load))
(add-hook 'c-mode-hook 'my:flymake-google-init)
(add-hook 'c++-mode-hook 'my:flymake-google-init)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flycheck                                                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'c++-mode-hook
          (lambda ()
            (setq flycheck-clang-language-standard "c++11")
            (setq flycheck-clang-include-path (list "/usr/local/include" "/usr/include"))
            (setq flycheck-c/c++-gcc-executable "/usr/bin/clang++")
            (flycheck-mode)))


;; clang include paths for flycheck
;;
(setq flycheck-clang-include-path
  (append (mapcar
            (lambda (item)
              (concat "-I" item))
            (split-string
              "
                /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1
                /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/8.0.0/include
                /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
                /usr/local/opt/opencv3/include
                /usr/local/include
                /usr/include
                "))
    flycheck-clang-include-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; google-c-style mode                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)


;;
;; code indentation
;;
(defun fix-c-indent-offset-according-to-syntax-context (key val)
  "KEY VAL Remove the old element."
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  ;; new value
  (add-to-list 'c-offsets-alist '(key . val)))

(add-hook 'c-mode-common-hook
  (lambda ()
    (when (derived-mode-p 'c-mode 'c++-mode)
      ;; indent
      (fix-c-indent-offset-according-to-syntax-context 'substatement 0)
      (fix-c-indent-offset-according-to-syntax-context 'func-decl-cont 0))))

;;
;; [ modern-cpp-font-lock ] -- font-locking for C++ mode.
;;
(use-package modern-cpp-font-lock
  :ensure t
  :init
  (modern-c++-font-lock-global-mode t))

;;----------------------------------------------------------------------------
(provide 'cpp-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; cpp-config.el ends here
