;;; package  --- ivy-config.el
;;;
;;; Commentary:
;;;
;;; Filename: ivy-config.el
;;; Description: Emacs incremental completion and selection narrowing framework
;;;              configuration file for IVY settings.
;;;
;;; elisp code for customizing the IVY settings
;;;
;;; Code:
;;;
;;;=============================================================================
(require-package 'ivy)
(setq ivy-use-virtual-buffers t)
(setq ivy-virtual-abbreviate 'full)
(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
(setq ivy-height 12)
(setq ivy-display-style 'fancy)
(setq ivy-count-format "[%d/%d] ")
(setq ivy-initial-inputs-alist nil)


(require-package 'swiper)
(after 'swiper
  (defadvice swiper (before dotemacs activate)
    (setq gc-cons-threshold most-positive-fixnum))
  (defadvice swiper-all (before dotemacs activate)
    (setq gc-cons-threshold most-positive-fixnum)))


(require-package 'counsel)


(after "projectile-autoloads"
  (require-package 'counsel-projectile))


(defmacro /ivy/propertize (prefix face)
  "Customize the PREFIX and FACE for ivy."
  `(lambda (str)
     (propertize str 'line-prefix ,prefix 'face ,face)))


(defun /ivy/mini ()
  "Ivy mini buffer settings."
  (interactive)
  (setq gc-cons-threshold most-positive-fixnum)
  (let* ((buffers (mapcar #'buffer-name (buffer-list)))
         (project-files
          (if (projectile-project-p)
              (mapcar (/ivy/propertize "[ project ] " 'ivy-virtual) (projectile-current-project-files))
            nil))
         (bufnames (mapcar (/ivy/propertize "[ buffer  ] " 'ivy-remote) buffers))
         (recents (mapcar (/ivy/propertize "[ recent  ] " 'ivy-subdir) recentf-list)))
    (ivy-read "Search: " (append project-files bufnames recents)
              :action (lambda (f)
                        (with-ivy-window
                          (cond ((member f bufnames)
                                 (switch-to-buffer f))
                                ((file-exists-p f)
                                 (find-file f))
                                (t
                                 (find-file (concat (projectile-project-root) f)))))))))

(defun /ivy/activate-as-switch-engine (on)
  "Check if the ivy navigation engine is ON."
  (if on
      (progn
        (counsel-mode t)
        (counsel-projectile-mode t)
        (ivy-mode t))
    (counsel-mode -1)
    (counsel-projectile-mode -1)
    (ivy-mode -1)))

(when (eq dotemacs-switch-engine 'ivy)
  (/ivy/activate-as-switch-engine t))

(provide 'ivy-config)

;;; ivy-config.el ends here
