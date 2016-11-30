;;; package --- customize fringe configuration for Emacs
;;;
;;; Filename   : frnge-config.el
;;; description: fringe display configuration for emacs
;;;
;;; Commentary:
;;             fringe customization
;;-------------------------------------------------------------------
;;;
;;; Code:
;;;
(setq indicate-buffer-boundaries 'right
      visual-line-fringe-indicators '(left-curly-arrow nil)
      left-fringe-width 8)
;;; Empty lines
;; (define-fringe-bitmap 'empty-line (vector 2 2 4 5 4 2 2) 8 8 '(center t))
;; (define-fringe-bitmap 'empty-line (vector 0 0 0 8) 8 4 '(top t))
;; (define-fringe-bitmap 'empty-line (vector 0 8 0 0 0 8 0 0) 8 8 '(top t))
(define-fringe-bitmap 'empty-line (vector 0 4 0 0) 8 4 '(top t))
;;; Top and bottom of buffer
;; Thin lines:
;(define-fringe-bitmap 'top-left-angle (vector 255 1 1 1 1 1 1 1) 8 8 'top)
;(define-fringe-bitmap 'bottom-left-angle (vector 1 1 1 1 1 1 1 255) 8 8 'bottom)
;(define-fringe-bitmap 'top-right-angle (vector 1 1 1 1 1 1 1 1) 8 8 'bottom)
;; More delicate, dotted lines:
;(define-fringe-bitmap 'top-left-angle (vector 85 0 1 0 1 0 0 0) 8 8 'top)
;(define-fringe-bitmap 'top-right-angle (vector 0 1 0 1 0 1 0 1) 8 8 'bottom)
;(define-fringe-bitmap 'bottom-left-angle (vector 0 0 0 1 0 1 0 85) 8 8 'bottom)
;(define-fringe-bitmap 'left-bracket (vector 85 0 1 0 0 1 0 85) 8 8 'center)
;; Delicate arrows:
(define-fringe-bitmap 'up-arrow (vector 8 20 34 65 0 0 0 0) 8 8 'top)
(define-fringe-bitmap 'down-arrow (vector 0 0 0 0 65 34 20 8) 8 8 'bottom)
;;; Arrows
;; Delicate, more visible but asymmetryc arrows:
;(define-fringe-bitmap 'up-arrow (vector 1 0 5 0 17 0 65 0) 8 8 'top)
;(define-fringe-bitmap 'down-arrow (vector 0 65 0 17 0 5 0 1) 8 8 'bottom)
;; Replacements for curly arrow for wrapped lines
;(define-fringe-bitmap 'left-curly-arrow (vector 5 0 5 0 0 0 0 0) 8 8 'top)
;(define-fringe-bitmap 'right-curly-arrow (vector 0 0 0 0 0 160 0 160) 8 8 'bottom)
;; Left and right arrows for truncated lines
;(define-fringe-bitmap 'right-arrow (vector 0 16 4 1 4 16 0 0) 8 8 'center)
;(define-fringe-bitmap 'left-arrow (vector 0 8 32 128 32 8 0 0) 8 8 'center)

(provide 'fringe-config)

;;; fringe-config.el ends here
