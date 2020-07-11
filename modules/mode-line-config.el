;;; package --- mode-line-config.el configuration settings for mode line
;; -*- mode: emacs-lisp -*-
;;;
;;; Commentary:
;;;
;;; Filename   : mode-line-config.el
;;; Description: Emacs Modeline themes with SML and Airline
;;;
;;; elisp code for customizing the Emacs mode line
;;;
;;; Code:
;;;

(require-package 'highlight-numbers)
(add-hook 'prog-mode-hook #'highlight-numbers-mode)

(require-package 'highlight-quoted)
(add-hook 'prog-mode-hook #'highlight-quoted-mode)


;;**
;; [fancy modeline(s) - powerline, airline and sml]
(use-package smart-mode-line-powerline-theme
  :ensure t
  :after powerline
  :after smart-mode-line
  ;; :config
  ;; (sml/setup)
  ;; (sml/apply-theme 'powerline)
  )

;;**
;; [define constants for holding the themes]
(defcustom aqua-airline-theme nil
  "Select an appropriate Airline Theme."
  :type 'symbol)

(defcustom aqua-sml-theme nil
  "Theme for smart-mode-line."
  :type 'symbol)

;;
;; [change the airline themes as required from the below]
(setq aqua-airline-theme 'airline-cool)

;;**
;; [set smart-mode-line theme]
(setq aqua-sml-theme 'dark)
;; alternative themes available:
;; (sml/apply-theme 'powerline)
;; (sml/apply-theme 'dark)
;; (sml/apply-theme 'light)
;; (sml/apply-theme 'respectful)
;; (sml/apply-theme 'automatic)


;;**
;; [vim airline theme for emacs modeline customized display]
(defun load-airline-settings ()
  "Load required Airline theme settings."
  (interactive)
  (require 'airline-themes)
  ;; do not display the current directory
  (setq airline-display-directory nil)
  ;; setting powerline fonts for font glyphs
  (setq powerline-utf-8-separator-left        #xe0b0
        powerline-utf-8-separator-right       #xe0b2
        airline-utf-glyph-separator-left      #xe0b0
        airline-utf-glyph-separator-right     #xe0b2
        airline-utf-glyph-subseparator-left   #xe0b1
        airline-utf-glyph-subseparator-right  #xe0b3
        airline-utf-glyph-branch              #xe0a0
        airline-utf-glyph-readonly            #xe0a2
        airline-utf-glyph-linenumber          #xe0a1))

(defun apply-airline ()
  "Apply the Airline Themes as needed."
  (interactive)
  (load-airline-settings)
  (load-theme aqua-airline-theme t))


;;**
;; [sml] - smart-mode-line enable/disable
(defun apply-smart-modeline ()
  "Apply SML Smart Model Line if selected."
  (interactive)
  (use-package smart-mode-line
    :ensure t
    :defer 0.2
    :config
    (add-hook 'after-load-theme-hook 'smart-mode-line-enable)
    (setq sml/no-confirm-load-theme t
          sml/theme 'dark
          sml/mode-width 'full
          sml/name-width 30
          sml/shorten-modes t)
    (if (not (null aqua-sml-theme))
        (setq sml/theme aqua-sml-theme)
      ;; delegate theme to the current active one
      (setq sml/theme 'dark))
    (sml/setup)
    (sml/apply-theme aqua-sml-theme)))


(defun apply-sml ()
  "Apply Smart Mode Line if needed."
  (interactive)
  (require-package 'smart-mode-line)
  (require 'smart-mode-line)
  (if (require 'smart-mode-line nil 'noerror)
      (progn
        ;;(setq powerline-arrow-shape 'curve)
        ;;(setq powerline-default-separator-dir '(right . left))
        (setq sml/name-width 30)
        (setq sml/no-confirm-load-theme t
              sml/theme 'dark
              sml/mode-width 'full
              sml/name-width 30
              sml/shorten-modes t)
        (rich-minority-mode 1)
        (if after-init-time
            (sml/setup)
          (add-hook 'after-init-hook #'sml/setup))
        (if (not (null aqua-sml-theme))
            (setq sml/theme aqua-sml-theme)
          ;; delegate theme to the current active one
          (setq sml/theme nil))
        (sml/apply-theme aqua-sml-theme)
        ;; (after 'evil
        ;;   (evil-sml-customize))
        )))

(defun evil-sml-customize ()
  "Customize mode line colors in evil mode."
  (defvar dotemacs--original-mode-line-bg (face-background 'mode-line))
  (defadvice evil-set-cursor-color (after dotemacs activate)
    (cond ((evil-emacs-state-p)
           (set-face-background 'mode-line "#440000"))
          ((evil-insert-state-p)
           (set-face-background 'mode-line "#F5ECCE"))
          ((evil-visual-state-p)
           (set-face-background 'mode-line "#440044"))
          (t
           (set-face-background 'mode-line dotemacs--original-mode-line-bg)))))


;;**
;; [airline] - uncomment if airline themes are required (load airline settings)
(when (eq dotemacs-mode-line 'airline)
  (add-hook 'after-init-hook 'apply-airline))

;;**
;; [sml] - uncomment if smart mode line is required
(when (eq dotemacs-mode-line 'sml)
  (add-hook 'after-init-hook 'apply-sml))

;;**
;; [spaceline] - uncomment if spaceline mode line is required
(when (eq dotemacs-mode-line 'spaceline)
  (require-package 'spaceline)
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state
        spaceline-minor-modes-separator nil)
  (spaceline-spacemacs-theme)
  (spaceline-info-mode)
  (after "helm-autoloads"
    (spaceline-helm-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;; mode line configuration end ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mode-line-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; mode-line-config.el ends here
