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
;;;===========================================================================
(require 'ycmd)             ; load package for ycmd
(require 'flycheck-ycmd)    ; flycheck integration
(require 'ycmd-eldoc)       ; adds eldoc support for ycmd-mode buffers
;(require 'company-ycm)     ; back-end for ycm

;;;
;;; Code:
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YCMD Configuration for Emacs                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-hook 'after-init-hook #'global-ycmd-mode)  ;; global mode
;; (set-variable 'ycmd-global-config
;;               "~/.emacs.d/global_config.py")
;; (set-variable 'ycmd-server-command
;;               '("/usr/local/bin/python3"
;;                 "~/sw/programming/python/YouCompleteMe/third_party/ycmd/ycmd"))


(eval-after-load "ycmd-autoloads"
  '(progn
     (set-variable 'ycmd-server-command
       '("/usr/local/bin/python3"
          ,(file-truename "~/sw/programming/python/YouCompleteMe/third_party/ycmd/ycmd/")))
     (set-variable 'ycmd-global-config
       (expand-file-name (concat user-emacs-directory "/.ycm_extra_conf.py")))))

;; ycm configuration file
(set-variable 'ycmd-extra-conf-whitelist '("~/sw/programming/*"))
(set-variable 'ycmd-python-binary-path "/usr/local/bin/python3")

;; force semantic completion for ycm
(set-variable 'ycmd-force-semantic-completion t)

;; time for re-parsing the file contents (default 0.5)
(setq 'ycmd-idle-change-delay 0.1)

;; show flycheck errors in the idle-mode / on focus
;(setq ycmd-parse-conditions '(save new-line mode-enabled idle-change))
(setq 'ycmd-parse-conditions '(save new-line buffer-focus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ycmd company integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'company-ycmd)                  ; company completion framework
(eval-after-load "company-ycmd-autoloads"
  '(progn (company-ycmd-setup))
  ;; if YCMD is used no need for default clang support
  (setq company-backends (remove 'company-clang company-backends))
  )

(defun ycmd-setup-completion-at-point-function ()
  "Setup `completion-at-point-functions' for `ycmd-mode'."
  (add-hook 'completion-at-point-functions
            #'ycmd-complete-at-point nil :local))

(add-hook 'ycmd-mode #'ycmd-setup-completion-at-point-function)

;; auto enable for c++ mode
(add-hook 'c++-mode-hook (lambda ()
                            (ycmd-mode)
                            ;(company-mode)
                            ))

;;
; flycheck integration
;;
(add-hook 'python-mode-hook
          (lambda () (add-to-list 'flycheck-disabled-checkers 'ycmd)))

(flycheck-ycmd-setup)

;; Make sure the flycheck cache sees the parse results
(add-hook 'ycmd-file-parse-result-hook 'flycheck-ycmd--cache-parse-results)

;; Add the ycmd checker to the list of available checkers
(add-to-list 'flycheck-checkers 'ycmd)

;;
; for eldoc integration
;;
(add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'ycm-config)

;;; ycm-config.el ends here
