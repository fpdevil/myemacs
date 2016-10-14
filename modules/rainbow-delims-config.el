;;===============================================================
;;; configuration file for rainbow delimiters
;; Filename: rainbow-delims-config.el
;; Description: A fancy mode for colored pairing of parenthesis
;;
;;; Commentary:
;;
;; elisp code for rainbow delimiter handling
;;===============================================================


;;
; fancy stuff
; for rainbow delimiters
;;
(require 'rainbow-delimiters)
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
;;
; fancy minor mode purely eye candy ((()))
;;
(setq minor-mode-alist
  `((rainbow-delimiters-mode " ")
    (rainbow-delimiters-mode #("(" 0 1 (face rainbow-delimiters-depth-1-face)))
    (rainbow-delimiters-mode #("(" 0 1 (face rainbow-delimiters-depth-2-face)))
    (rainbow-delimiters-mode #("(" 0 1 (face rainbow-delimiters-depth-3-face)))
    (rainbow-delimiters-mode #(")" 0 1 (face rainbow-delimiters-depth-3-face)))
    (rainbow-delimiters-mode #(")" 0 1 (face rainbow-delimiters-depth-2-face)))
    (rainbow-delimiters-mode #(")" 0 1 (face rainbow-delimiters-depth-1-face)))
    ,@(assq-delete-all 'rainbow-delimiters-mode minor-mode-alist)))
;; fancy mode end
(add-hook 'python-mode-hook 'rainbow-delimiters-mode)
(add-hook 'shell-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(provide 'rainbow-delims-config)


;;; rainbow-delims-config ends here