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
(require 'helm-projectile)        ; helm ui for projectile

;;;
;;; Code:
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Map pairs of simultaneously pressed keys to commands                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'key-chord)
(key-chord-define-global "fm" 'list-buffers)
(key-chord-define-global "fm" 'helm-mini)

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
      helm-M-x-fuzzy-match t
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-split-window-in-side-p t)

;;----------------------------------------------------------------------------
;; global kbd mapping (from prelude emacs - Thanks to Batsov)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-command-map (kbd "o")     'helm-occur)
(define-key helm-command-map (kbd "g")     'helm-do-grep)
(define-key helm-command-map (kbd "C-c w") 'helm-wikipedia-suggest)
(define-key helm-command-map (kbd "SPC")   'helm-all-mark-rings)

;; personal mappings
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)           ; list files
(global-set-key (kbd "C-x b")   'helm-mini)                 ; list current opened buffers
(global-set-key (kbd "C-x C-r") 'helm-recentf)              ; list recently opened files
(global-set-key (kbd "C-h f" )  'helm-apropos)              ; apropos helm style
(global-set-key (kbd "C-c i")   'helm-imenu)                ; imenu
(global-set-key (kbd "M-y")     'helm-show-kill-ring)       ; kill-ring
(global-set-key (kbd "C-h C-l") 'helm-locate-library)       ; locate library
;(key-chord-define-global "fm"   'helm-mini)

(require 'helm-describe-modes)      ; Helm interface to Emacs’s describe-mode
(global-set-key [remap describe-mode] #'helm-describe-modes)


;; using helm for listing the eshell history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (substitute-key-definition 'eshell-list-history 'helm-eshell-history eshell-mode-map)))

;;
; enable modes
;;
(helm-mode 1)
(helm-popup-tip-mode 1)

(provide 'helm-settings-config)
;;; helm-settings-config.el ends here
