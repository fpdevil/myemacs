;;; package  --- projectile-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : projectile-config.el
;;; Description: Emacs configuration for Projectile
;;;              Projectile is a project management mode
;;;              Custom settings for handling the projects
;;;
;;; elisp code for customizing the projectile settings
;;;
;;; Code:
;;;
;;;
(lazy-init
 (require-package 'projectile)                           ; load package for projectile
 (require-package 'helm-projectile)                      ; helm ui for projectile

 ;; shows Pr[project name] in minibuffer if inside a project
 (after 'projectile (setq-default projectile-mode-line
                                  '(:eval (if (file-remote-p default-directory) " Pr" (format
                                                                                       " Pr[%s]"
                                                                                       (projectile-project-name))))))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Project Interaction Library for Emacs                                    ;;
 ;; C-c p h or M-x helm-projectile                             	             ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (helm-projectile-on)

 (setq projectile-enable-caching t)
 (setq projectile-require-project-root nil) ;; use projectile everywhere (no .projectile file needed)
 (setq projectile-switch-project-action 'projectile-dired)
 (setq projectile-remember-window-configs t )
 (setq projectile-completion-system 'helm)
 (setq projectile-switch-project-action 'helm-projectile)
 (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" cache-dir))
 (setq projectile-cache-file (expand-file-name "projectile.cache" cache-dir))

 (projectile-mode t)

 (add-to-list 'projectile-globally-ignored-directories "elpa")
 (add-to-list 'projectile-globally-ignored-directories ".cache")
 )

(provide 'projectile-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; projectile-config.el ends here
