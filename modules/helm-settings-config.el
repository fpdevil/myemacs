;;; package  --- helm-settings-config.el
;;;
;;; Commentary:
;;;
;;; Filename: helm-settings-config.el
;;; Description: Emacs incremental completion and selection narrowing framework
;;;              configuration file for HELM settings.
;;;              A major/minor mode for helm based help utilities
;;;
;;; elisp code for customizing the helm settings
;;;===========================================================================
(require 'helm-config)
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm set custom variables                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq helm-input-idle-delay 0.1
      helm-yas-display-key-on-candidate t
      helm-recentf-fuzzy-match t
      helm-ff-file-name-history-use-recentf t
      helm-buffers-fuzzy-matching t
      helm-completion-in-region-fuzzy-match t
      helm-mode-fuzzy-match t
      helm-M-x-fuzzy-match t)

;;
; global kbd mapping
;;
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)           ; list files
(global-set-key (kbd "C-x b")   'helm-mini)                 ; list current opened buffers
(global-set-key (kbd "C-x C-r") 'helm-recentf)              ; list recently opened files
(global-set-key (kbd "C-c i")   'helm-imenu)                ; imenu
(global-set-key (kbd "M-y")     'helm-show-kill-ring)       ; kill-ring
;(key-chord-define-global "fm"   'helm-mini)

;;
; enable modes
;;
(helm-mode 1)
(helm-popup-tip-mode 1)

(provide 'helm-settings-config)

;;; helm-settings-config.el ends here
