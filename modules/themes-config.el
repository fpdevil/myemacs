;;; package --- themes-config.el configuration settings for color theme
;; -*- mode: emacs-lisp -*-
;;;
;;; Commentary:
;;;
;;; Filename   : themes-config.el
;;; Description: Emacs Color themes set or unset values
;;;              Add an airline theme or a smart mode line
;;;
;;; elisp code for customizing the color theme
;;;
;;; Code:
;;;
;;;

(require-package 'color-theme)

;; with this setting all themes are safe to me
(setq custom-safe-themes t)

;;------------------------------------------------------------------------------
;; adapted from the prelude
;; If you don't customize it, this is the theme you get.
;;------------------------------------------------------------------------------
(setq-default custom-enabled-themes
              '(
                ;;sanityinc-tomorrow-bright
                ;;doom-molokai
                ;;sanityinc-tomorrow-day
                ))

;;------------------------------------------------------------------------------
;; Ensure that themes will be applied even if they have not been customized
;;------------------------------------------------------------------------------
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))


;;-------------------------------------------------------------------------------
;; apply the themes after Emacs initializes
;;-------------------------------------------------------------------------------
(setq color-theme-illegal-faces "^\\(w2-\\|dropdown-\\|info-\\|linum\\|yas-\\|font-lock\\|dired-directory\\)")
;; to fix the aquamacs theme setup issue
;; (setq default-frame-alist nil)
(add-hook 'after-init-hook 'reapply-themes)

;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))

;; For zerodark theme
;;-------------------------------------------------------------------------------
;; enable the zerodark theme
;;-------------------------------------------------------------------------------
(defun zerodark ()
  "Enable the `Zerodark' theme."
  (interactive)
  ;;(require 'zerodark-theme)
  (let ((frame-name (cdr (assq 'name (frame-parameters (selected-frame))))))
    (call-process-shell-command
     (format
      "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT 'dark' -name '%s'"
      frame-name)))
  (load-theme 'zerodark t)
  (zerodark-setup-modeline-format)
  ;;(set-selected-frame-dark)
  )

;; enable zerodark
;;(add-hook 'after-init-hook 'zerodark)

;;-------------------------------------------------------------------------------
;; Toggle between spacemacs's light and dark themes
;;-------------------------------------------------------------------------------
(defun space-light ()
  "Activate spacemacs light theme."
  (interactive)
  (load-theme 'spacemacs-light t))


(defun space-dark ()
  "Activate spacemacs light theme."
  (interactive)
  (load-theme 'spacemacs-dark t))

;;-------------------------------------------------------------------------------
;; enable moe's theme
;; automatically switch colors between dark and light based on the system time
;;-------------------------------------------------------------------------------
;; (require 'moe-theme-switcher)
(defun moe ()
  "Activate moe light theme."
  (interactive)
  (require 'powerline)
  (require 'moe-theme)
  (powerline-moe-theme)
  (setq moe-theme-highlight-buffer-id t
        moe-theme-resize-markdown-title '(1.5 1.4 1.3 1.2 1.0 1.0)
        moe-theme-resize-org-title '(1.5 1.4 1.3 1.2 1.1 1.0 1.0 1.0 1.0)
        moe-theme-resize-rst-title '(1.5 1.4 1.3 1.2 1.1 1.0))
  (moe-theme-set-color 'orange)
  (moe-light))

;;(add-hook 'after-init-hook 'moe)

;;-------------------------------------------------------------------------------
;;** theme for org-mode
;;-------------------------------------------------------------------------------
(use-package org-beautify-theme
  :after org
  :ensure t)

;;-------------------------------------------------------------------------------
;;** variable pitch for org-mode and markdown-mode
;;-------------------------------------------------------------------------------
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Monaco for Powerline" :height 140 :weight light))))
 '(fixed-pitch ((t (:family "Inconsolata for Powerline" :slant normal :weight normal :height 1.0 :width normal)))))
(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'markdown-mode-hook 'variable-pitch-mode)

(message "now loading themes-config.el...")
;;;;;;;;;;;;;;;;;;;;;;; color theme configuration end ;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'themes-config)
;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions)
;; End:
;;; themes-config.el ends here
