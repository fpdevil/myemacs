;;; package  --- ido-config.el
;;;
;;; Commentary:
;;;
;;; Filename: ido-config.el
;;; Description: Emacs incremental completion and selection narrowing framework
;;;              configuration file for IDO settings.
;;;              A major/minor mode for IDO based help utilities
;;;
;;; elisp code for customizing the IDO settings
;;;
;;; Code:
;;;
;;;=============================================================================
(setq ido-enable-prefix nil)
(setq ido-use-virtual-buffers t)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-use-filename-at-point 'guess)
(setq ido-save-directory-list-file (concat cache-dir "ido.last"))

(after 'ido
  (require-package 'ido-ubiquitous)

  (require-package 'flx-ido)
  (flx-ido-mode t)

  (require-package 'ido-vertical-mode)
  (ido-vertical-mode))

(defun /ido/activate-as-switch-engine (on)
  "Switch to IDO mode if ON."
  (if on
      (progn
        (ido-mode t)
        (ido-everywhere t)
        (ido-ubiquitous-mode t))
    (ido-mode -1)
    (ido-everywhere -1)
    (ido-ubiquitous-mode -1)))

(when (eq dotemacs-switch-engine 'ido)
  (/ido/activate-as-switch-engine t))

(provide 'ido-config)

;;; ido-config.el ends here
