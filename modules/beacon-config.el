;;; package  --- beacon-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : beacon-config.el
;;; Description: Emacs package for highlighting the cursors position
;;;              Beacon — Never lose your cursor again
;;;              This is a global minor-mode
;;; elisp code for customizing the beacon
;;;==========================================================================
(require 'beacon) ;; show the cursor when moving after big movements in the window
;;;
;;; Code:
;;;

(beacon-mode +1)

(provide 'beacon-config)
;;; beacon-config.el ends here
