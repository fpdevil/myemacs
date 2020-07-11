;;; package --- customize python configuration for Emacs
;;;
;;; Commentary:
;;; Filename: popwin-config.el
;;; Description: popup window manager for Emacs
;;;              Treat some windows as popups and close them when they
;;;              are not used anymore
;;;
;;; Code:
;;;
;;;

(require 'popwin)
(popwin-mode 1)

;; focus help popup so we can easily exit it with popwin
(setq help-window-select t)

(defun popwin-config/popup-downloads ()
  "Pop up the downloads buffer (4th eshell buffer for me"
  (interactive)
  (when (not (get-buffer "*eshell downloads*"))
    (let ((eshell-buffer-name "*eshell downloads*"))
      (eshell)))
  (popwin:popup-buffer "*eshell downloads*"))

;; show a term buffer in a popup window with M-x popwin-term:term
(defun popwin-term:term ()
  "Show term buffers in a popup window."
  (interactive)
  (popwin:display-buffer-1
   (or (get-buffer "*terminal*")
       (save-window-excursion
         (call-interactively 'term)))
   :default-config-keywords '(:position :top)))

(progn
  ;; don't use default value but manage it ourselves
  (setq popwin:special-display-config nil)

  ;; buffers that we manage
  (push '("*Help*"                 :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
  (push '("*compilation*"          :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
  (push '("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("*Async Shell Command*"  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("*undo-tree*"            :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
  (push '("*ert*"                  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("*grep*"                 :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("*nosetests*"            :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '(term-mode                             :position        :stick t               :height 16 ) popwin:special-display-config)
  (push '("*ielm*"                                               :stick t                          ) popwin:special-display-config)
  (push '("*eshell pop*"                                         :stick t                          ) popwin:special-display-config)
  (push '("*Python*"                                             :stick t                          ) popwin:special-display-config)
  (push '("*Python Help*"                                        :stick t               :height 20 ) popwin:special-display-config)
  (push '("*jedi:doc*"                                           :stick t :noselect t              ) popwin:special-display-config)
  ;; clojure cider
  (push '("*cider-error*"          :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
  (push '("*cider-doc*"            :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
  ;; Haskell
  (push '("*haskell*"                                            :stick t                          ) popwin:special-display-config)
  (push '("*GHC Info*"                                                                             ) popwin:special-display-config)
  (push '("^\*WoMan.+\*$" :regexp t             :position bottom                                   ) popwin:special-display-config)

  (global-set-key (kbd "C-x M-d") #'popwin-config/popup-downloads)
  (global-set-key (kbd "C-h e") 'popwin:messages)
  ;;(global-set-key (kbd "C-z p") popwin:keymap)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'popwin-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; popwin-config.el ends here
