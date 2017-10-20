;;; package --- custom settings under aqua-customizations.el
;;;
;;; Commentary:
;;;
;;; filename.  : aqua-customizations.el
;;; description: Contains general Aquamacs custom options
;;;              This is the place where Aquqmacs default preferences are
;;;              changed or altered.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code:
;;;

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
;; CEDET ede custom save directory settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ede-simple-save-directory
      (expand-file-name "EDE" cache-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; escape save question during exit for custom settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq aquamacs-save-options-on-quit nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save and load mini buffer history
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq savehist-file
      (expand-file-name "minibuffer-history.el" cache-dir))
(when (file-exists-p savehist-file)
  (load savehist-file))

(setq history-length 100)
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
;-------------------------------------------------------------------------------
;; Enable emoji, and stop the UI from freezing when trying to display them.
;-------------------------------------------------------------------------------
(if (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))


(message "Loaded the aquq-customizations...")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aqua-customizations)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; aqua-customizations.el ends here
