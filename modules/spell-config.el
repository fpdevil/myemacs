;;; package  --- spell-config.el
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : spell-config.el
;;; Description: Emacs enable spell checking for comments & text
;;;              spell checking on the fly.
;;;===========================================================================
;;; Code:

(defvar spell-checking-enable-by-default t
  "Enable spell checking by default.")

(defvar spell-checking-enable-auto-dictionary nil
  "Specify if auto-dictionary should be enabled or not.")

(defvar enable-flyspell-auto-completion nil
  "If not nil, show speeling suggestions in popups.")

(defun spell-config/add-flyspell-hook (hook)
  "Add `flyspell-mode' to the given HOOK, if
`spell-checking-enable-by-default' is true."
  (when spell-checking-enable-by-default
    (add-hook hook 'flyspell-mode)))

(defun spell-config/change-dictionary ()
  "Change the dictionary. Use the ispell version if
auto-dictionary is not used, use the adict version otherwise."
  (interactive)
  (if (fboundp 'adict-change-dictionary)
      (adict-change-dictionary)
    (call-interactively 'ispell-change-dictionary)))


(defun spell-config/init-auto-dictionary ()
  (use-package auto-dictionary
    :defer t
    :init
    (progn
      (add-hook 'flyspell-mode-hook 'auto-dictionary-mode)
      ;; set the local buffer dictionary if set
      ;; auto-dictionary will replace it with a guessed one at each activation
      (defun adict-set-local-dictionary ()
        "Set the local dictionary if not nil."
        (when (and (fboundp 'adict-change-dictionary)
                ispell-local-dictionary)
          (adict-change-dictionary ispell-local-dictionary)))
      (add-hook 'auto-dictionary-mode-hook
                'adict-set-local-dictionary 'append))))


(defun spell-config/init-flyspell ()
  (use-package flyspell
    :defer t
    :commands (spell-checking/change-dictionary)
    :init
    (progn
      (spell-checking/add-flyspell-hook 'text-mode-hook)
      (when spell-checking-enable-by-default
        (add-hook 'prog-mode-hook 'flyspell-prog-mode))
      (add-toggle-spelling-checking
        :status flyspell-mode
        :on (if (derived-mode-p 'prog-mode)
              (flyspell-prog-mode)
              (flyspell-mode))
        :off (progn
               (flyspell-mode-off)
               ;; disable auto-dictionary when disabling spell-checking
               (when (fboundp 'auto-dictionary-mode) (auto-dictionary-mode -1)))
        :documentation "Enable automatic spell checking."
        :evil-leader "tS"))))

(defun spell-config/init-flyspell-correct ()
  (use-package flyspell-correct
    :commands (flyspell-correct-word-generic
               flyspell-correct-previous-word-generic)
    :init
    (evil-leader/set-key "Sc" 'flyspell-correct-previous-word-generic)))

(defun spell-config/init-flyspell-correct-helm ()
  (use-package flyspell-crrect-helm
    :commands (flyspell-correct-helm)
    :init
    (setq flyspell-correct-interface #'flyspell-correct-popup)))

(defun spell-config/init-flyspell-correct-popup ()
  (use-package flyspell-correct-popup
    :commands (flyspell-correct-popup)
    :init
    (setq flyspell-correct-interface #'flyspell-correct-popup)))

(defun spell-config/init-flyspell-popup ()
  (use-package flyspell-popup
    :defer t
    :init
    (progn
      (setq flyspell-popup-correct-delay 0.8)
      (add-hook 'flyspell-mode-hook 'flyspell-popup-auto-correct-mode))))


;; enable for org-mode
(spell-config/add-flyspell-hook 'org-mode-hook)


(provide 'spell-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; spell-config.el ends here
