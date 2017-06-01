;;; package --- customize python configuration for Emacs
;;;
;;; Commentary:
;;; Filename: popwin-config.el
;;; Description: popup window manager for Emacs
;;;              Treat some windows as popups and close them when they
;;;              are not used anymore
;;;===========================================================================
(require 'popwin)
;;;
;;; Code:
;;;
(popwin-mode 1)
(add-to-list 'popwin:special-display-config '"*jedi:doc*")

(progn
  (defvar popwin:special-display-config-backup popwin:special-display-config)
  (setq display-buffer-function 'popwin:display-buffer)

  ;; basic
  (push '("*Help*" :stick t :noselect t) popwin:special-display-config)
  (push '("*Pp Eval Output*" :stick t) popwin:special-display-config)

  ;; compilation
  (push '(compilation-mode :stick t :width 0.4) popwin:special-display-config)

  ;; quickrun
  (push '("*quickrun*" :stick t) popwin:special-display-config)

  ;; dictionaly
  (push '("*dict*" :stick t) popwin:special-display-config)
  (push '("*sdic*" :stick t) popwin:special-display-config)

  ;; popwin for slime
  (push '(slime-repl-mode :stick t) popwin:special-display-config)

  ;; man
  (push '(Man-mode :stick t :height 20) popwin:special-display-config)

  ;; Elisp
  (push '("*ielm*" :stick t) popwin:special-display-config)
  (push '("*eshell pop*" :stick t) popwin:special-display-config)

  ;; python
  (push '("*Python*"   :stick t) popwin:special-display-config)
  (push '("*Python Help*" :stick t :height 20) popwin:special-display-config)
  (push '("*jedi:doc*" :stick t :noselect t) popwin:special-display-config)

  ;; Haskell
  (push '("*haskell*" :stick t) popwin:special-display-config)
  (push '("*GHC Info*") popwin:special-display-config)

  ;; git-gutter
  (push '("*git-gutter:diff*" :width 0.5 :stick t)
        popwin:special-display-config)

  ;; es-results-mode
  (push '(es-result-mode :stick t :width 0.5)
        popwin:special-display-config)

  ;; direx
  (push '(direx:direx-mode :position left :width 40 :dedicated t)
        popwin:special-display-config)

  (push '("*Occur*" :stick t) popwin:special-display-config)

  ;; org-mode
  (push '("*Org tags*" :stick t :height 30)
        popwin:special-display-config)

  ;; Completions
  (push '("*Completions*" :stick t :noselect t) popwin:special-display-config)

   ;; async shell commands
  (push '("*Async Shell Command*" :stick t) popwin:special-display-config)

  (defun my/popup-downloads ()
    "Pop up the downloads buffer (4th eshell buffer for me"
    (interactive)
    (when (not (get-buffer "*eshell downloads*"))
      (let ((eshell-buffer-name "*eshell downloads*"))
        (eshell)))
    (popwin:popup-buffer "*eshell downloads*"))

  ;; eshell 4 is always my "download stuff" buffer
  (global-set-key (kbd "C-x M-d") #'my/popup-downloads)
  (global-set-key (kbd "C-h e") 'popwin:messages))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'popwin-config)
;;; popwin-config.el ends here
