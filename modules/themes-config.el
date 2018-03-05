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
;;===============================================================================
(require 'color-theme)

;;-------------------------------------------------------------------------------
;; setting default color theme to required (from Steve Purcell .emacs)
;;-------------------------------------------------------------------------------
(setq-default custom-enabled-themes '(
                                      ;;darkokai
                                      ;;majapahit-light
                                      leuven
                                      ;;deeper-blue
                                      ;;material
                                      ;;material-light
                                      ;;spolsky
                                      ;;badger
                                      ))

;;-------------------------------------------------------------------------------
;; helper function's for disabling and enabling themes
;;-------------------------------------------------------------------------------
(defun change-theme (theme)
  "Disable the current theme if any and load the THEME."
  (interactive
   (list
    (intern (completing-read "Load a custom theme: "
                             (mapc 'symbol-name
                                   (custom-available-themes))))))
  (let ((enabled-themes custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(defun disable-active-themes ()
  "Disable the currently active themes listed from `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

;;-------------------------------------------------------------------------------
;; Ensure that themes will be applied even if they have not been customized
;;-------------------------------------------------------------------------------
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (message "loading the theme %s" theme)
      (load-theme theme t)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

;;-------------------------------------------------------------------------------
;; Toggle between light and dark solarized themes
;;-------------------------------------------------------------------------------
(defun light ()
  "Activate a solarized light color theme."
  (interactive)
  (require 'color-theme-sanityinc-solarized)
  (color-theme-sanityinc-solarized-light))

(defun dark ()
  "Activate a solarized dark color theme."
  (interactive)
  (require 'color-theme-sanityinc-tomorrow)
  (color-theme-sanityinc-solarized-dark))

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
  (set-selected-frame-dark))

;;-------------------------------------------------------------------------------
;; Toggle between prelude's light and dark solarized themes
;;-------------------------------------------------------------------------------
(defun prelude-solarized-settings ()
  "Settings for the prelude solarized themes."
  (interactive)
  ;; make the fringe stand out from the background
  (setq solarized-distinct-fringe-background t
        ;; Don't change the font for some headings and titles
        solarized-use-variable-pitch nil
        ;; make the modeline high contrast
        solarized-high-contrast-mode-line t
        solarized-distinct-doc-face t
        ;; Use less bold font
        solarized-use-less-bold t
        ;; Use more italics
        solarized-use-more-italic t
        ;; Use less colors for indicators such as git:gutter, flycheck and similar
        solarized-emphasize-indicators nil
        ;; Don't change size of org-mode headlines (but keep other size-changes)
        solarized-scale-org-headlines nil
        ;; Avoid all font-size changes
        solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0)
  (message "loading the custom settings for solarized..."))

(defun slight ()
  "Activate prelude solarized light theme."
  (interactive)
  (require 'solarized-theme)
  (prelude-solarized-settings)
  (load-theme 'solarized-light t))

(defun sdark ()
  "Activate prelude solarized light theme."
  (interactive)
  (require 'solarized-theme)
  (prelude-solarized-settings)
  (load-theme 'solarized-dark t))

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

;; (moe)

;;-------------------------------------------------------------------------------
;; apply the themes after Emacs initializes
;;-------------------------------------------------------------------------------
(setq color-theme-illegal-faces "^\\(w2-\\|dropdown-\\|info-\\|linum\\|yas-\\|font-lock\\|dired-directory\\)")
(add-hook 'after-init-hook 'reapply-themes)

;; set the background color for the header.
(custom-set-faces '(header-line ((t (:background "#003366")))))

;;;;;;;;;;;;;;;;;;;;;;; color theme configuration end ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'themes-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; themes-config.el ends here
