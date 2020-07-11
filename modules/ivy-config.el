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

  ;; enhance fuzzy matching
  (use-package flx)
  ;; Enhance M-x
  (use-package amx)
  ;; More friendly display transformer for Ivy
  (use-package ivy-rich
    :defines (all-the-icons-dir-icon-alist bookmark-alist)
    :functions (all-the-icons-icon-family
                all-the-icons-match-to-alist
                all-the-icons-auto-mode-match?
                all-the-icons-octicon
                all-the-icons-dir-is-submodule)
    :preface
    (defun ivy-rich-bookmark-name (candidate)
      (car (assoc candidate bookmark-alist)))

    (defun ivy-rich-repo-icon (candidate)
      "Display repo icons in `ivy-rich`."
      (all-the-icons-octicon "repo" :height .9))

    (defun ivy-rich-org-capture-icon (candidate)
      "Display repo icons in `ivy-rich`."
      (pcase (car (last (split-string (car (split-string candidate)) "-")))
        ("emacs" (all-the-icons-fileicon "emacs" :height .68 :v-adjust .001))
        ("schedule" (all-the-icons-faicon "calendar" :height .68 :v-adjust .005))
        ("tweet" (all-the-icons-faicon "commenting" :height .7 :v-adjust .01))
        ("link" (all-the-icons-faicon "link" :height .68 :v-adjust .01))
        ("memo" (all-the-icons-faicon "pencil" :height .7 :v-adjust .01))
        (_       (all-the-icons-octicon "inbox" :height .68 :v-adjust .01))
        ))

    (defun ivy-rich-org-capture-title (candidate)
      (let* ((octl (split-string candidate))
             (title (pop octl))
             (desc (mapconcat 'identity octl " ")))
        (format "%-25s %s"
                title
                (propertize desc 'face `(:inherit font-lock-doc-face)))))

    (defun ivy-rich-buffer-icon (candidate)
      "Display buffer icons in `ivy-rich'."
      (when (display-graphic-p)
        (when-let* ((buffer (get-buffer candidate))
                    (major-mode (buffer-local-value 'major-mode buffer))
                    (icon (if (and (buffer-file-name buffer)
                                   (all-the-icons-auto-mode-match? candidate))
                              (all-the-icons-icon-for-file candidate)
                            (all-the-icons-icon-for-mode major-mode))))
          (if (symbolp icon)
              (setq icon (all-the-icons-icon-for-mode 'fundamental-mode)))
          (unless (symbolp icon)
            (propertize icon
                        'face `(
                                :height 1.1
                                        :family ,(all-the-icons-icon-family icon)
                                        ))))))

    (defun ivy-rich-file-icon (candidate)
      "Display file icons in `ivy-rich'."
      (when (display-graphic-p)
        (let ((icon (if (file-directory-p candidate)
                        (cond
                         ((and (fboundp 'tramp-tramp-file-p)
                               (tramp-tramp-file-p default-directory))
                          (all-the-icons-octicon "file-directory"))
                         ((file-symlink-p candidate)
                          (all-the-icons-octicon "file-symlink-directory"))
                         ((all-the-icons-dir-is-submodule candidate)
                          (all-the-icons-octicon "file-submodule"))
                         ((file-exists-p (format "%s/.git" candidate))
                          (all-the-icons-octicon "repo"))
                         (t (let ((matcher (all-the-icons-match-to-alist candidate all-the-icons-dir-icon-alist)))
                              (apply (car matcher) (list (cadr matcher))))))
                      (all-the-icons-icon-for-file candidate))))
          (unless (symbolp icon)
            (propertize icon
                        'face `(
                                :height 1.1
                                        :family ,(all-the-icons-icon-family icon)
                                        ))))))

    :hook (ivy-rich-mode . (lambda ()
                             (setq ivy-virtual-abbreviate
                                   (or (and ivy-rich-mode 'abbreviate) 'name))))

    :init
    (setq ivy-rich-display-transformers-list
          '(ivy-switch-buffer
            (:columns
             ((ivy-rich-buffer-icon)
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand)))
            ivy-switch-buffer-other-window
            (:columns
             ((ivy-rich-buffer-icon)
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand)))
            counsel-M-x
            (:columns
             ((counsel-M-x-transformer (:width 40))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
            counsel-describe-function
            (:columns
             ((counsel-describe-function-transformer (:width 45))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
            counsel-describe-variable
            (:columns
             ((counsel-describe-variable-transformer (:width 45))
              (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
            counsel-find-file
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate)))
            counsel-file-jump
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate)))
            counsel-dired-jump
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate)))
            counsel-git
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate)))
            counsel-recentf
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate (:width 110))))
            counsel-bookmark
            (:columns
             ((ivy-rich-bookmark-type)
              (ivy-rich-bookmark-name (:width 30))
              (ivy-rich-bookmark-info (:width 80))))
            counsel-projectile-switch-project
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate)))
            counsel-fzf
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate)))
            ivy-ghq-open
            (:columns
             ((ivy-rich-repo-icon)
              (ivy-rich-candidate)))
            ivy-ghq-open-and-fzf
            (:columns
             ((ivy-rich-repo-icon)
              (ivy-rich-candidate)))
            counsel-projectile-find-file
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate)))
            counsel-org-capture
            (:columns
             ((ivy-rich-org-capture-icon)
              (ivy-rich-org-capture-title)
              ))
            counsel-projectile-find-dir
            (:columns
             ((ivy-rich-file-icon)
              (counsel-projectile-find-dir-transformer)))))

    (setq ivy-rich-parse-remote-buffer nil)
    :config
    (ivy-rich-mode 1))

  )

(provide 'ivy-config)

;;; ivy-config.el ends here
