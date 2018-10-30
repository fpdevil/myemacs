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
;;;

(when (eq dotemacs-switch-engine 'ivy)
  (use-package swiper
    :bind
    ("C-s" . counsel-grep-or-swiper)
    ("H-s" . swiper-all)
    :config
    (ivy-mode 1))

  (use-package counsel
    :init
    (require 'ivy)
    (setq projectile-completion-system 'ivy)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-use-selectable-prompt t)
    (setq ivy-display-style 'fancy)
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
    (setq ivy-count-format "[%d/%d] ")
    (setq ivy-initial-inputs-alist nil)
    (define-prefix-command 'counsel-prefix-map)
    (global-set-key (kbd "C-c") 'counsel-prefix-map)
    :bind
    (("M-x"     . counsel-M-x)
     ("C-x b"   . ivy-switch-buffer)
     ("C-x C-f" . counsel-find-file)
     ("C-x l"   . counsel-locate)
     ("C-h f"   . counsel-describe-function)
     ("C-h v"   . counsel-describe-variable)
     ("C-h i"   . counsel-info-lookup-symbol)
     ("C-c r"   . ivy-resume)
     ("C-c i"   . counsel-imenu)
     ("C-c l"   . counsel-load-library)
     ("C-c t"   . counsel-load-theme)
     ("C-c g"   . counsel-git)
     ("C-c e"   . counsel-git-grep)
     ("C-c a"   . counsel-ag))
    :config
    (progn
      (counsel-mode)
      (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
      (define-key ivy-minibuffer-map (kbd "M-<SPC>") 'ivy-dispatching-done)
      ;; C-RET call and go to next
      (define-key ivy-minibuffer-map (kbd "C-<return>")
        (lambda ()
          "Apply action and move to next/previous candidate."
          (interactive)
          (ivy-call)
          (ivy-next-line)))

      ;; M-RET calls action on all candidates to end.
      (define-key ivy-minibuffer-map (kbd "M-<return>")
        (lambda ()
          "Apply default action to all candidates."
          (interactive)
          (ivy-beginning-of-buffer)
          (loop for i from 0 to (- ivy--length 1)
                do
                (ivy-call)
                (ivy-next-line)
                (ivy--exhibit))
          (exit-minibuffer)))

      ;; s-RET to quit
      (define-key ivy-minibuffer-map (kbd "s-<return>")
        (lambda ()
          "Exit with no action."
          (interactive)
          (ivy-exit-with-action
           (lambda (x) nil))))

      ;; Show keys
      (define-key ivy-minibuffer-map (kbd "?")
        (lambda ()
          (interactive)
          (describe-keymap ivy-minibuffer-map)))

      (define-key ivy-minibuffer-map (kbd "<left>") 'ivy-backward-delete-char)
      (define-key ivy-minibuffer-map (kbd "<right>") 'ivy-alt-done)
      (define-key ivy-minibuffer-map (kbd "C-d") 'ivy-backward-delete-char)))

  (use-package ivy-hydra
    :ensure t)
  )

(provide 'ivy-config)

;;; ivy-config.el ends here
