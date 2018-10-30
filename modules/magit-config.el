;;; package --- Magit and plugins for Emacs
;;;
;;; Commentary:
;;; Filename: magit-config.el
;;; description: customizations for magit family
;;;
;;; Code:
;;;


(defun me/unboldify (&optional faces)
  "Set the weight property of FACES to `normal'.
If FACES is not provided or nil, use `face-list' instead."
  (interactive)
  (mapc (lambda (face)
          (when (eq (face-attribute face :weight) 'bold)
            (set-face-attribute face nil :weight 'normal)))
        (or faces (face-list))))

(use-package magit
  :preface
  (defun me/magit-display-buffer-same (buffer)
    "Display most magit popups in the current buffer."
    (display-buffer
     buffer
     (cond ((and (derived-mode-p 'magit-mode)
                 (eq (with-current-buffer buffer major-mode) 'magit-status-mode))
            nil)
           ((memq (with-current-buffer buffer major-mode)
                  '(magit-process-mode
                    magit-revision-mode
                    magit-diff-mode
                    magit-stash-mode))
            nil)
           (t '(display-buffer-same-window)))))
  :config

  ;; Use better defaults
  (setq-default
   magit-display-buffer-function 'me/magit-display-buffer-same
   magit-diff-highlight-hunk-body nil
   magit-diff-highlight-hunk-region-functions
   '(magit-diff-highlight-hunk-region-dim-outside
     magit-diff-highlight-hunk-region-using-face)
   magit-popup-display-buffer-action '((display-buffer-same-window))
   magit-refs-show-commit-count 'all
   magit-section-show-child-count t)

  ;; Customize lighters
  (delight
   '((magit-diff-mode "Magit Diff")
     (magit-log-mode "Magit Log")
     (magit-popup-mode "Magit Popup")
     (magit-status-mode "Magit Status")))

  ;; Customize faces
  (set-face-attribute 'magit-diff-file-heading-highlight nil :background nil)
  (set-face-attribute 'magit-diff-hunk-region nil :inherit 'region)
  (set-face-attribute 'magit-popup-heading nil :height me/font-size-title)
  (set-face-attribute 'magit-section-heading nil :height me/font-size-title)
  (set-face-attribute 'magit-section-highlight nil :background nil)
  (me/unboldify '(magit-branch-current
                  magit-branch-local
                  magit-branch-remote
                  magit-head
                  magit-refname
                  magit-refname-stash
                  magit-refname-wip
                  magit-tag)))

(use-package magit-blame
  :ensure nil
  :config (me/unboldify '(magit-blame-summary)))

(use-package magit-diff
  :ensure nil
  :config
  (me/unboldify '(magit-diff-file-heading
                  magit-diff-file-heading-highlight
                  magit-diff-file-heading-selection)))

(use-package magit-popup
  :defer t
  :ensure nil
  :config
  (me/unboldify '(
                  magit-popup-argument
                  magit-popup-heading
                  magit-popup-key
                  magit-popup-option-value
                  )))

(use-package magit-section
  :ensure nil
  :config
  (me/unboldify '(magit-section-heading
                  magit-section-heading-selection
                  magit-section-secondary-heading)))

(use-package gitattributes-mode :delight gitattributes-mode "Git Attributes")
(use-package gitconfig-mode :delight gitconfig-mode "Git Config")
(use-package gitignore-mode :delight gitignore-mode "Git Ignore")


(provide 'magit-config)

;;; magit-config.el ends here
