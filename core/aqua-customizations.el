;;; package --- custom settings under aqua-customizations.el
;;;
;;; Commentary:
;;;
;;; filename.  : aqua-customizations.el
;;; description: Contains general Aquamacs custom options
;;;              This is the place where Aquamacs default preferences are
;;;              changed or altered.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code:
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utf-8 character set encoding and Locale
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prefer-coding-system         'utf-8)
(set-default-coding-systems   'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; language setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq current-language-environment "English")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Sampath Singamsetty")
(setq user-mail-address "Singamsetty.Sampath@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-indent on RET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map (kbd "RET") 'newline-and-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disable startup, splash screen and startup message
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-screen t)
;; (setq inhibit-startup-message t)
;; (setq initial-scratch-message nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visually indicate buffer boundaries and scrolling in the fringe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default indicate-buffer-boundaries '((top . left) (t . right)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abbrev Mode - Saving abbreviations at custom location
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq abbrev-file-name
      (expand-file-name "abbrev_defs" cache-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; escape save question during exit for custom settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq aquamacs-save-options-on-quit nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save and load mini buffer history
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(savehist-mode 1)
(setq savehist-file (expand-file-name "minibuffer-history.el" cache-dir))
;; (when (file-exists-p savehist-file)
;;   (load savehist-file))

(setq history-length 100)
(setq savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)

(setq aquamacs-scratch-file
    (expand-file-name "scratch buffer" "~/.emacs.d/preferences"))

(setq save-frame-position-file
    (expand-file-name "frame-positions.el" "~/.emacs.d/preferences"))

(setq save-place-file
    (expand-file-name "places.el" "~/.emacs.d/preferences"))

(setq desktop-path (quote ("~/aquamacs.d" "." "~")))

(setq ede-simple-save-directory
 (expand-file-name "EDE" "~/.emacs.d/preferences/"))

;; save-list-file customizations
(let ((dir (expand-file-name (concat cache-dir "/auto-save-list/"))))
  (setq auto-save-list-file-prefix (concat dir "saves-"))
  (setq auto-save-file-name-transforms `((".*" ,(concat dir "save-") t))))

;; better scrolling
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t
      scroll-margin 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable emoji, and stop the UI from freezing when trying to display them.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aqua-customizations)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; aqua-customizations.el ends here
