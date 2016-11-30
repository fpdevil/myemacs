;;; package  --- whichkey-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : whichkey-config.el
;;; Description: Emacs package that displays available keybindings in popup
;;;              https://github.com/justbur/emacs-which-key
;;; elisp code for customizing the which-key settings
;;;==============================================================
(require 'which-key)
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs package that displays available keybindings in popup               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(which-key-mode)
;; set the time delay (in seconds) for the which-key popup to appear.
(setq which-key-idle-delay 0.4)
;; set the separator used between keys and descriptions.
(setq which-key-separator " → " )
;; allow extra padding for Unicode character
(setq which-key-unicode-correction 3)

(provide 'whichkey-config)

;;; whichkey-config.el ends here
