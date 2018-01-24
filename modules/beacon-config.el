;;; package  --- beacon-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : beacon-config.el
;;; Description: Emacs package for highlighting the cursors position
;;;              Beacon — Never lose your cursor again
;;;              This is a global minor-mode
;;; elisp code for customizing the beacon
;;;
;;; Code:
;;;
;;;==========================================================================
(require 'beacon) ;; show the cursor when moving after big movements in the window

(after "beacon"
  (progn
    ;; params to control when beacon should blink at all
    (setq beacon-blink-when-point-moves-vertically nil)     ;; default nil
    (setq beacon-blink-when-point-moves-horizontally nil)   ;; default nil
    (setq beacon-blink-when-buffer-changes t)               ;; default t
    (setq beacon-blink-when-window-scrolls t)               ;; default t
    (setq beacon-blink-when-window-changes t)               ;; default t
    (setq beacon-blink-when-focused nil)                    ;; default nil
    ;; duration params
    (setq beacon-blink-duration 0.3)                        ;; default 0.3
    (setq beacon-blink-delay 0.3)                           ;; default 0.3
    ;; appearance params
    (setq beacon-size 30)                                   ;; default 40
    (setq beacon-color 0.5)                                 ;; default 0.5
    ;; push the mark for you whenever point moves a long distance
    (setq beacon-push-mark 35)                              ;; default 35
    ;; enable globally
    (beacon-mode +1)))

(provide 'beacon-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; beacon-config.el ends here
