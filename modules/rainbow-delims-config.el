;;; package --- rainbow-delims-config
;;; -*- coding: utf-8 -*-
;;;
;;; Commentary:
;;; Filename: rainbow-delims-config.el
;;; Description: fancy stuff, configuration file for rainbow delimiters
;;;              a fancy mode for colored pairing of parenthesis
;;;              colors can be picked from http://colours.neilorangepeel.com/
;;;
;;; Code:
;;;

(require 'rainbow-delimiters)      ; rainbow delimiters package

;;; rainbow delimiters custom color codes
;;; fancy (but useful) stuff for rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; for automatically setting default more saturated colors...
;; (require 'cl-lib)
;; (require 'color)
;; (cl-loop
;;  for index from 1 to rainbow-delimiters-max-face-count
;;  do
;;  (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
;;    (cl-callf color-saturate-name (face-foreground face) 30)))

;; make unmatched parens display in bold red and with a strike through
;; (require 'paren) ; show-paren-mismatch is defined in paren.el
;; (set-face-attribute 'rainbow-delimiters-unmatched-face nil
;;                     :foreground 'unspecified
;;                     :inherit 'error
;;                     :strike-through t)



;; some customizations for the default modes
(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#008080"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#bf62a6"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#f28c33"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#87cefa"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#ffd700"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#79c267"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#483d8b"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#4682b4"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#c71585"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground nil :background nil :underline "#cc0033" :height 1.2))))

 ;; rainbow-delimiters-mode setup, with decreasing bracket size
 ;; '(rainbow-delimiters-depth-1-face ((t (:foreground "#008080" :height 1.4))))
 ;; '(rainbow-delimiters-depth-2-face ((t (:foreground "#bf62a6" :height 1.3))))
 ;; '(rainbow-delimiters-depth-3-face ((t (:foreground "#f28c33" :height 1.3))))
 ;; '(rainbow-delimiters-depth-4-face ((t (:foreground "#87cefa" :height 1.2))))
 ;; '(rainbow-delimiters-depth-5-face ((t (:foreground "#ffd700" :height 1.2))))
 ;; '(rainbow-delimiters-depth-6-face ((t (:foreground "#79c267" :height 1.1))))
 ;; '(rainbow-delimiters-depth-7-face ((t (:foreground "#483d8b" :height 1.1))))
 ;; '(rainbow-delimiters-depth-8-face ((t (:foreground "#4682b4" :height 1.0))))
 ;; '(rainbow-delimiters-depth-9-face ((t (:foreground "#c71585" :height 1.0))))
 ;; '(rainbow-delimiters-unmatched-face ((t (:foreground nil :background nil :underline "#cc0033" :height 0.9))))
 )

;;** fancy minor mode purely eye candy ((()))
(defun fancy-rbow-modeline ()
  "A fancy minor mode display of rainbow parenthesis."
  (interactive)
  (setq minor-mode-alist
        `((rainbow-delimiters-mode " ")
          (rainbow-delimiters-mode #("(" 0 1 (face rainbow-delimiters-depth-1-face)))
          (rainbow-delimiters-mode #("(" 0 1 (face rainbow-delimiters-depth-2-face)))
          (rainbow-delimiters-mode #("(" 0 1 (face rainbow-delimiters-depth-3-face)))
          (rainbow-delimiters-mode #(")" 0 1 (face rainbow-delimiters-depth-3-face)))
          (rainbow-delimiters-mode #(")" 0 1 (face rainbow-delimiters-depth-2-face)))
          (rainbow-delimiters-mode #(")" 0 1 (face rainbow-delimiters-depth-1-face)))
          ,@(assq-delete-all 'rainbow-delimiters-mode minor-mode-alist))))

;; uncomment below to display ((())) in the mode line
;; (fancy-rbow-modeline)

;; enable rainbow mode for the below
;;(add-hook 'python-mode-hook 'rainbow-delimiters-mode)
;;(add-hook 'c-mode-hook 'rainbow-delimiters-mode)
;;(add-hook 'c++-mode-hook 'rainbow-delimiters-mode)
;;(add-hook 'go-mode-hook 'rainbow-delimiters-mode)
;;(add-hook 'haskell-mode-hook 'rainbow-delimiters-mode)
;;(add-hook 'erlang-mode-hook 'rainbow-delimiters-mode)
;;(add-hook 'elixir-mode-hook 'rainbow-delimiters-mode)
;;(add-hook 'shell-mode-hook 'rainbow-delimiters-mode)
;;(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(provide 'rainbow-delims-config)

;;; rainbow-delims-config.el ends here
