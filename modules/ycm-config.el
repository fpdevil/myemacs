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
(require 'company-ycmd)     ; company completion framework
;(require 'company-ycm)     ; back-end for ycm
(require 'ycmd-eldoc)       ; adds eldoc support for ycmd-mode buffers
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YCMD Configuration for Emacs                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (add-hook 'after-init-hook #'global-ycmd-mode)  ;; global mode
; (add-hook 'c++-mode-hook 'ycmd-mode)            ;; only for cpp

(set-variable 'ycmd-global-config
              "/Users/sampathsingamsetty/.emacs.d/global_config.py")
(set-variable 'ycmd-server-command
              '("/usr/local/bin/python3"
                "/Users/sampathsingamsetty/sw/programming/python/YouCompleteMe/third_party/ycmd/ycmd"))
; ycm configuration file
(set-variable 'ycmd-extra-conf-whitelist
              '("/Users/sampathsingamsetty/sw/programming/*"))
(set-variable 'ycmd-python-binary-path
              "/usr/local/bin/python3")
(set-variable 'ycmd-force-semantic-completion t)

;; show flycheck errors in idle-mode
(setq ycmd-parse-conditions '(save new-line mode-enabled idle-change))

;;
; company integration
;;
(defun ycmd-setup-completion-at-point-function ()
  "Setup `completion-at-point-functions' for `ycmd-mode'."
  (add-hook 'completion-at-point-functions
            #'ycmd-complete-at-point nil :local))

(add-hook 'ycmd-mode #'ycmd-setup-completion-at-point-function)

;; for c++ mode
(add-hook 'c++-mode-hook (lambda ()
                            (ycmd-mode )
                            (company-mode)))


;; Enable company-ycm.
; (add-to-list 'company-backends 'company-ycm)
; (add-to-list 'company-begin-commands 'c-electric-colon)
; (add-to-list 'company-begin-commands 'c-electric-lt-gt)


;;
; flycheck integration
;;
(flycheck-ycmd-setup)
;; Make sure the flycheck cache sees the parse results
(add-hook 'ycmd-file-parse-result-hook 'flycheck-ycmd--cache-parse-results)

;; Add the ycmd checker to the list of available checkers
(add-to-list 'flycheck-checkers 'ycmd)


;;
; for eldoc
;;
(add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)


(provide 'ycm-config)

;;; ycm-config.el ends here
