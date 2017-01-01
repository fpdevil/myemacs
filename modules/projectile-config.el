;;; package  --- projectile-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : projectile-config.el
;;; Description: Emacs configuration for Projectile
;;;              Custom settings for handling the projects
;;;
;;; elisp code for customizing the projectile settings
;;;===========================================================================
(require 'projectile)             ; load package for projectile
(require 'helm-projectile)        ; helm ui for projectile
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Interaction Library for Emacs                                    ;;
;; C-c p h or M-x helm-projectile                        	                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(helm-projectile-on)
(projectile-global-mode t)
(setq projectile-enable-caching t)
(setq projectile-require-project-root nil)  ;; use projectile everywhere (no .projectile file needed)
(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-remember-window-configs t )
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)

(provide 'projectile-config)

;;; projectile-config.el ends here
