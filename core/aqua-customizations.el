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
;; Abbrev Mode - Saving abbreviations at custom location
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq abbrev-file-name
      (expand-file-name "abbrev_defs" save-dir))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CEDET ede custom save directory settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ede-simple-save-directory
      (expand-file-name "EDE" save-dir))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save and load mini buffer history
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq savehist-file
      (expand-file-name "minibuffer-history.el" save-dir))
(when (file-exists-p savehist-file)
  (load savehist-file))

(setq auto-save-list-file-name
      (expand-file-name ".saves-" "~/.emacs.d/cache/auto-save-list"))

(message "Loaded the aquq-customizations...")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'aqua-customizations)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; aqua-customizations.el ends here
