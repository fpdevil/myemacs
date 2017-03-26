;;; package  --- whichkey-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : whichkey-config.el
;;; Description: Emacs package that displays available keybindings in popup
;;;              https://github.com/justbur/emacs-which-key
;;; elisp code for customizing the which-key settings
;;;==========================================================================
(require 'which-key)
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs package that displays available keybindings in popup               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(which-key-mode +1)
;; set the time delay (in seconds) for the which-key popup to appear.
(setq which-key-idle-delay 0.4
      ;; set the separator used between keys and descriptions.
      which-key-separator " → "
      ;; allow extra padding for Unicode character
      which-key-unicode-correction 3
      which-key-sort-order 'which-key-key-order-alpha
      which-key-key-replacement-alist '(("<\\([[:alnum:]-]+\\)>" . "\\1")
                                        ("left" . "◀")
                                        ("right" . "▶")
                                        ("up" . "▲")
                                        ("down" . "▼")))

(provide 'whichkey-config)

;;; whichkey-config.el ends here
