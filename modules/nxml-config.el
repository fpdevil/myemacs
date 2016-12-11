;;; package  --- nxml-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : nxml-config.el
;;; Description: Editing XML in Emacs
;;;
;;; elisp code for customizing the nxml settings
;;; http://lgfang.github.io/mynotes/emacs/emacs-xml.html
;;;===========================================================================
(require 'cl)
(require 'nxml-mode)
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nXML mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt
                                  '("xml" "xsd" "sch" "rng"
                                    "xslt" "svg" "rss"
                                    "html" "jelly" "jnlp" "fo"
                                    "xsl" "xslt") t)
                           "\\'") 'nxml-mode))

(when (> emacs-major-version 21)
  (setq magic-mode-alist
        (cons '("<\\?xml " . nxml-mode) magic-mode-alist)))

(fset 'xml-mode 'nxml-mode)
(fset 'html-mode 'nxml-mode)
(require 'rng-loc nil t)

;; encoding, auto complete etc...
(setq nxml-slash-auto-complete-flag t
      nxml-auto-insert-xml-declaration-flag t
      nxml-slash-auto-complete-flag t
      nxml-default-buffer-file-coding-system 'utf-8)

;;
; hide show
;;
(add-hook 'nxml-mode-hook (lambda() (hs-minor-mode 1)))

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>" ;; regexp for start block
               "-->\\|</[^/>]*[^/]>" ;; regexp for end block
               "<!--"
               nxml-forward-element
               nil))

;;
; xpath display where are we in buffer
;;
(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path, from http://www.emacswiki.org/emacs/NxmlMode."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while
            (and (< (point-min) (point)) ;; Doesn't error if point is at
                                         ;; beginning of buffer
                 (condition-case nil
                     (progn
                       (nxml-backward-up-element) ; always returns nil
                       t)
                   (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/")))))))

;;
; key-bindings
;;
(defun lgfang-toggle-level ()
  "Mainly to be used in nxml mode."
  (interactive) (hs-show-block) (hs-hide-level 1))
(eval-after-load "nxml-mode"
  '(progn
     (define-key nxml-mode-map (kbd "M-'") 'lgfang-toggle-level)
     (define-key nxml-mode-map [mouse-3] 'lgfang-toggle-level)))

;;
; for auto-complete of nXml
;;
(require 'auto-complete-nxml nil t)
;; customize
;; Keystroke for popup help about something at point.
;(setq auto-complete-nxml-popup-help-key "C-:")
;; Keystroke for toggle on/off automatic completion.
;(setq auto-complete-nxml-toggle-automatic-key "C-c C-t")
;; If you want to start completion manually from the beginning
;(setq auto-complete-nxml-automatic-p nil)

(provide 'nxml-config)

;;; nxml-config.el ends here
