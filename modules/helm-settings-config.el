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

;;
; global kbd mapping
;;
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
; listing files
(global-set-key (kbd "C-x C-f") 'helm-find-files)
; listing opened buffers
(global-set-key (kbd "C-x b")   'helm-mini)
(global-set-key (kbd "C-h r")   'helm-info-emacs)
;(key-chord-define-global "fm"   'helm-mini)

;;
; enable modes
;;
(helm-mode 1)
(helm-popup-tip-mode 1)


(provide 'helm-settings-config)
;;; helm-settings-config.el ends here
