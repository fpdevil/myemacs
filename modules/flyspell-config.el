;;; package  --- flyspell-config.el
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;;
;;; Filename   : flyspell-config.el
;;; Description: Emacs enable spell checking for comments & text
;;;              spell checking on the fly.
;;;
;;; Code:
;;;
;;;

(require 'flyspell)

;; FOR FLYSPELL MODE
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

;; flyspell line styles
;; (custom-set-faces
;;  '(flyspell-duplicate ((t (:underline (:color "Blue" :style wave)))))
;;  '(flyspell-incorrect ((t (:underline (:color "Purple" :style wave))))))
(after 'flyspell
  (set-face-attribute 'flyspell-duplicate nil
                      :foreground "white"
                      :background "orange" :box t :underline t)
  (set-face-attribute 'flyspell-incorrect nil
                      :foreground "white"
                      :background "red" :box t :underline t))

(use-package flyspell-lazy
  :config
  (flyspell-lazy-mode 1)
  :custom
  (flyspell-lazy-changes-threshold 10)
  (flyspell-lazy-idle-seconds 1)
  (flyspell-lazy-less-feedback t)
  (flyspell-lazy-mode t)
  (flyspell-lazy-size-threshold 5)
  (flyspell-lazy-use-flyspell-word nil)
  (flyspell-lazy-window-idle-seconds 3))

;; improve performance by not printing messages for every word
(setq flyspell-issue-message-flag nil
      flyspell-issue-welcome-flag nil
      flyspell-use-meta-tab nil)

;; flyspell popup configuration
(use-package flyspell-correct-popup
  ;;:bind ("C-M-;" . flyspell-correct-wrapper)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-popup))

(use-package flyspell-popup
  :defer t
  ;;(define-key flyspell-mode-map (kbd "C-:") #'flyspell-popup-correct)
  :init
  (progn
    (setq flyspell-popup-correct-delay 0.8)
    (add-hook 'flyspell-mode-hook 'flyspell-popup-auto-correct-mode)))

;; Add hooks as needed
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; Force flyspell mode in non-programming modes
(defun turn-on-flyspell ()
   "Force flyspell-mode on using a positive arg.  For use in hooks."
   (interactive)
   (flyspell-mode 1))

;; better interface for corrections
(use-package flyspell-correct-ivy
  :ensure t
  :demand t
  :bind (:map flyspell-mode-map
              ("C-c $" . flyspell-correct-at-point)))

(use-package auto-dictionary
  :after flyspell
  :init
  (progn
  (setq ispell-local-dictionary "en_GB")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
  ;;(add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1)))
  (add-hook 'auto-dictionary-mode-hook
            (lambda ()
              (when (and
                     (fboundp 'adict-change-dictionary)
                     ispell-local-dictionary)
                (adict-change-dictionary ispell-local-dictionary))) 'append)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'flyspell-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; no-byte-compile t
;; End:

;;; flyspell-config.el ends here
