;;; package  --- ycm-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : ycm-config.el
;;; Description: Emacs configuration for YouCompleteMe
;;;              YouCompleteMe is a Python based static analyzer for intelligent
;;;              auto-completion(s)
;;;
;;; elisp code for customizing ycm
;;; https://github.com/abingham/emacs-ycmd
;;;
;;; Code:
;;;
;;;

(when (eq dotemacs-completion-engine 'company)

  (defcustom dotemacs-company/ycmd-server-command nil
    "The path to the ycmd package."
    :group 'dotemacs-company
    :type 'boolean)

  (require 'ycmd)             ; load package for ycmd
  (require 'flycheck-ycmd)    ; flycheck integration
  (require 'ycmd-eldoc)       ; adds eldoc support for ycmd-mode buffers
  (require 'company-ycmd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;     YCMD Configuration for Emacs                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let ((ycmd-dir (expand-file-name "private/youcompleteme/" user-emacs-directory)))
    (if (not (file-exists-p ycmd-dir))
        (progn
          (message "Cloning and building YouCompleteMe ...")
          (setenv "EXTRA_CMAKE_ARGS"
                  "-DPATH_TO_LLVM_ROOT=/opt/software/clang+llvm-7.0.0-x86_64-apple-darwin")
          (shell-command (concat "git clone --recursive "
                                 "https://github.com/Valloric/ycmd.git "
                                 ycmd-dir
                                 " && cd "
                                 ycmd-dir
                                 " && python3 build.py "
                                 "--clang-completer "
                                 "--system-libclang "
                                 "--gocode-completer "
                                 "--tern-completer"))
          (message "ycmd building/compilation done..."))
      (message "YouCompleteMe already exists; not cloning"))
    (set-variable 'ycmd-server-command
                  (list "/usr/local/bin/python3" (concat ycmd-dir "ycmd/"))))

  ;; for project specific configurations under "~/sw/programming/python/*"
  (set-variable 'ycmd-extra-conf-whitelist '("~/sw/programming/python/*"))
  ;; global ycmd configuration file
  (set-variable 'ycmd-global-config "~/.emacs.d/.ycm_extra_conf.py")
  (set-variable 'ycmd-global-modes 'nil)
  (set-variable 'ycmd-parse-conditions
                '(save new-line mode-enabled idle-change buffer-focus))
  (set-variable 'ycmd-python-binary-path (executable-find "python3"))

  (setq ycmd-extra-conf-handler 'load)
  (setq ycmd-force-semantic-completion t)
  (setq ycmd-min-num-chars-for-completion 1)
  (setq ycmd-gocode-binary-path "/usr/local/bin/go"
        ycmd-godef-binary-path "~/sw/programming/gocode/go/bin/godef")
  (setq global-ycmd-mode 'nil)


  ;; (add-hook 'after-init-hook 'global-ycmd-mode)


  ;; force semantic completion for ycm
  (set-variable 'ycmd-force-semantic-completion t)
  ;; time for re-parsing the file contents (default 0.5)
  (set-variable 'ycmd-idle-change-delay 0.1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ycmd company integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (after "company"
    (add-to-list (make-local-variable 'company-backends)
                 'company-ycmd)

    (after-load "company-ycmd-autoloads"
      '(progn (company-ycmd-setup)))

    ;; if YCMD is used no need for default clang support
    (setq company-backends (remove 'company-clang company-backends))
    ;; ycmd company mode hook
    (add-hook 'ycmd-mode-hook 'company-ycmd-setup))

  ;; auto-completion setup without company mode
  (defun ycmd-setup-completion-at-point-function ()
    "Setup `completion-at-point-functions' for `ycmd-mode'."
    (add-hook 'completion-at-point-functions
              #'ycmd-complete-at-point nil :local))

  (add-hook 'ycmd-mode #'ycmd-setup-completion-at-point-function)

  ;;{{{ auto enable only for c++ mode
  ;; You  can  effectively  disable  a   mode  by  removing  it  from  the
  ;; auto-mode-alist  variable.  Once  removed,  the mode  will  never  be
  ;; automatically chosen by emacs but it may still be started manually
  (rassq-delete-all 'ycmd-mode auto-mode-alist)
  ;;(add-hook 'c++-mode-hook (lambda () (ycmd-mode)))
  ;;}}}

  ;; flycheck integration
  (after-load 'flycheck
    (add-hook 'python-mode-hook
              (lambda () (add-to-list 'flycheck-disabled-checkers 'ycmd)))
    (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup)
    ;; (flycheck-ycmd-setup)
    ;; Make sure the flycheck cache sees the parse results
    (add-hook 'ycmd-file-parse-result-hook 'flycheck-ycmd--cache-parse-results)
    ;; Add the ycmd checker to the list of available checkers
    (add-to-list 'flycheck-checkers 'ycmd))

  ;; for eldoc integration
  (after-load 'eldoc
    (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'ycm-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; ycm-config.el ends here
