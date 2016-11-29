;;; package --- ecb configuration settings
;;;
;;; Commentary:
;;; Filename   : ecb-config.el
;;; Description: Emacs Code Browser
;;;
;;; elisp code for customizing the ECB
;;===========================================================================
(require 'ecb)
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ecb (emacs code browser)                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ecb-auto-activate nil)
(setq ecb-layout-name "left13")
(setq ecb-new-ecb-frame nil)
(setq ecb-tip-of-the-day nil)
(setq ecb-windows-width 13)

(provide 'ecb-config)

;;; ecb-config.el ends here