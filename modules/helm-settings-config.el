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
;;;
;;; Code:
;;;
;;;=============================================================================
(require 'helm-config)


;;------------------------------------------------------------------------------
;;; Map pairs of simultaneously pressed keys to commands
;;------------------------------------------------------------------------------
(require 'key-chord)
(key-chord-define-global "fm" 'list-buffers)
(key-chord-define-global "fm" 'helm-mini)

;;------------------------------------------------------------------------------
;;; helm set custom variables
;;------------------------------------------------------------------------------
(setq helm-bookmark-show-location t)
(setq helm-buffer-max-length 40)

;; -- for helm sources
(after 'helm-source
  (defun my-helm-make-source (f &rest args)
    (let ((source-type (cadr args))
          (props (cddr args)))
      (unless (child-of-class-p source-type 'helm-source-async)
        (plist-put props :fuzzy-match t))
      (apply f args)))
  (advice-add 'helm-make-source :around 'my-helm-make-source))

(after 'helm
    (require-package 'helm-descbinds)
    (require-package 'helm-flx)
      (helm-flx-mode t)
    (require-package 'helm-fuzzier)
        (helm-fuzzier-mode t)
    (require-package 'helm-dash)
      (setq helm-dash-browser-func 'eww)

    (require-package 'helm-ag)
    (setq helm-ag-fuzzy-match t)
    (after 'helm-ag
     (cond ((executable-find "ag")
               t)
             ((executable-find "pt")
              (setq helm-ag-base-command "pt -e --nogroup --nocolor"))
             ((executable-find "ack")
              (setq helm-ag-base-command "ack --nogroup --nocolor"))))

    (setq helm-swoop-pre-input-function #'ignore)
    (setq helm-swoop-use-line-number-face t)
    (setq helm-swoop-split-with-multiple-windows t)
    (setq helm-swoop-speed-or-color t)
    (setq helm-swoop-use-fuzzy-match t)
    (require-package 'helm-swoop)

    (after "projectile-autoloads"
        (require-package 'helm-projectile))

    (setq helm-input-idle-delay 0.1
            helm-yas-display-key-on-candidate t
            helm-recentf-fuzzy-match t
            helm-completion-in-region-fuzzy-match t
            helm-mode-fuzzy-match t
            helm-M-x-fuzzy-match t
            helm-always-two-windows t
            helm-echo-input-in-header-line t
            helm-split-window-in-side-p t
            helm-buffers-fuzzy-matching t
            helm-move-to-line-cycle-in-source nil
            helm-ff-search-library-in-sexp t
            helm-ff-file-name-history-use-recentf t
            helm-prevent-escaping-from-minibuffer t
            helm-display-header-line nil
            helm-imenu-execute-action-at-once-if-one nil
            helm-lisp-fuzzy-completion t
            helm-org-format-outline-path t
            helm-autoresize-min-height 10
            helm-autoresize-max-height 30
))

;;------------------------------------------------------------------------------
;; global kbd mapping (from prelude emacs - Thanks to Bbatsov)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;;------------------------------------------------------------------------------
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-command-map (kbd "o")     'helm-occur)
(define-key helm-command-map (kbd "g")     'helm-do-grep)
(define-key helm-command-map (kbd "C-c w") 'helm-wikipedia-suggest)
(define-key helm-command-map (kbd "SPC")   'helm-all-mark-rings)

;; personal mappings
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)           ;; list files
(global-set-key (kbd "C-x b")   'helm-mini)                 ;; list current opened buffers
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)         ;; list of buffers
(global-set-key (kbd "C-x C-r") 'helm-recentf)              ;; list recently opened files
(global-set-key (kbd "C-h f" )  'helm-apropos)              ;; apropos helm style
(global-set-key (kbd "C-c i")   'helm-imenu)                ;; imenu
(global-set-key (kbd "M-y")     'helm-show-kill-ring)       ;; kill-ring
(global-set-key (kbd "C-h r")   'helm-info-emacs)           ;; Emacs info
(global-set-key (kbd "C-h C-l") 'helm-locate-library)       ;; locate library
;(key-chord-define-global "fm"   'helm-mini)

(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

(require 'helm-describe-modes)      ; Helm interface to Emacs’s describe-mode
(global-set-key [remap describe-mode] #'helm-describe-modes)


;; using helm for listing the eshell history
(require 'helm-eshell)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (substitute-key-definition 'eshell-list-history
                                         'helm-eshell-history eshell-mode-map)))

;;
; enable modes
;;
(helm-mode 1)
(helm-popup-tip-mode 1)

;;------------------------------------------------------------------------------

(provide 'helm-settings-config)
;;; helm-settings-config.el ends here
