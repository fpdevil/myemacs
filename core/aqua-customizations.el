;;; package --- custom settings under aqua-customizations.el
;;;
;;; Commentary:
;;;
;;; filename.  : aqua-customizations.el
;;; description: Contains general Aquamacs custom options
;;;              This is the place where Aquamacs default preferences are
;;;              changed or altered.
;;; Code:
;;;


;;**
;;** utf-8 character set encoding and Locale
(prefer-coding-system         'utf-8)
(set-default-coding-systems   'utf-8)


;;**
;;** proper default language setup [EN]
(setq current-language-environment "English")


;;**
;;** auto-indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)

;;**
;;** disable startup, splash screen and startup message
(setq inhibit-startup-screen t)
;; (setq inhibit-startup-message t)
;; (setq initial-scratch-message nil)


;;**
;;** Visually indicate buffer boundaries and scrolling in the fringe
(setq-default indicate-buffer-boundaries '((top . left) (t . right)))

;;** Abbrev Mode - Saving abbreviations at custom location
(setq abbrev-file-name
      (expand-file-name "abbrev_defs" cache-dir))

;; abbrev mode for text mode
(add-hook 'text-mode-hook 'abbrev-mode)

;;**
;;** escape save question during exit for custom settings
(setq aquamacs-save-options-on-quit nil)

;;**
;;** save and load the mini buffer history
(savehist-mode 1)
(setq history-delete-duplicates t)
(setq savehist-file (expand-file-name "minibuffer-history.el" cache-dir))
;; (when (file-exists-p savehist-file)
;;   (load savehist-file))

(setq history-length 100)
(setq savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)

;;(setq aquamacs-scratch-file (expand-file-name "scratch buffer" preferences-dir))
;;(setq save-frame-position-file (expand-file-name "frame-positions.el" preferences-dir))
;;(setq save-place-file (expand-file-name "places.el" preferences-dir))
;;(setq ede-simple-save-directory (expand-file-name "EDE" preferences-dir))


(setq desktop-path (quote ("~/.emacs.d" "." "~")))

(setq aquamacs-autosave-directory
 (expand-file-name "AutoSave" cache-dir))


;;**
;;** save-list-file customizations
(let ((dir (expand-file-name (concat cache-dir "/auto-save-list/"))))
  (setq auto-save-list-file-prefix (concat dir "saves-"))
  (setq auto-save-file-name-transforms `((".*" ,(concat dir "save-") t))))

;;**
;;** for better scrolling
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t
      scroll-margin 3)

;;**
;;** [Emoji] - Enable and stop the UI from freezing when trying to display them.
(if (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

;;**
;;** replace region when typing
;; When Delete Selection mode is enabled, typed text replaces the selection
;; if the selection is active.  Otherwise, typed text is just inserted at
;; point regardless of any selection.  Also, commands that normally delete
;; just one character will delete the entire selection instead.
(delete-selection-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aqua-customizations)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; aqua-customizations.el ends here
