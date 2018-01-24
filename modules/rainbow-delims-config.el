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
;;;===========================================================================
(require 'rainbow-delimiters)      ; rainbow delimiters package

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rainbow delimiters custom color codes                                  ;;;
;;; fancy (but useful) stuff for rainbow delimiters                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; some customizations for the default modes
;; stolen from https://ogbe.net/emacsconfig.html
(set-face-attribute 'rainbow-delimiters-depth-1-face nil
                    :foreground "#78c5d6")
(set-face-attribute 'rainbow-delimiters-depth-2-face nil
                    :foreground "#bf62a6")
(set-face-attribute 'rainbow-delimiters-depth-3-face nil
                    :foreground "#459ba8")
(set-face-attribute 'rainbow-delimiters-depth-4-face nil
                    :foreground "#e868a2")
(set-face-attribute 'rainbow-delimiters-depth-5-face nil
                    :foreground "#79c267")
(set-face-attribute 'rainbow-delimiters-depth-6-face nil
                    :foreground "#f28c33")
(set-face-attribute 'rainbow-delimiters-depth-7-face nil
                    :foreground "#c5d647")
(set-face-attribute 'rainbow-delimiters-depth-8-face nil
                    :foreground "#f5d63d")
(set-face-attribute 'rainbow-delimiters-depth-9-face nil
                    :foreground "#78c5d6")
(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :foreground "#800000")

;;
; fancy minor mode purely eye candy ((()))
;;
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
