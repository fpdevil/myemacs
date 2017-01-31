;;; package  --- xslt-process-config.el
;;;
;;; Commentary:
;;;
;;; Filename   : xslt-process-config.el
;;; Description: XSLT transformation support handling in Emacs
;;;
;;; elisp code for customizing the xslt settings
;;; http://xslt-process.sourceforge.net/documentation.php
;;;===========================================================================
(require 'cl)
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for xslt-process using saxon or xalan processor                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(add-to-list 'load-path (expand-file-name "xslt-process/lisp" emacs-packages-dir))
;; set up xslt-process minor mode
(autoload 'xslt-process-mode "xslt-process" "Emacs XSLT processing" t)
(autoload 'xslt-process-install-docbook "xslt-process"
  "Register the DocBook package with XSLT-process" t)

(add-hook 'xml-mode-hook 'xslt-process-mode)
(add-hook 'xsl-mode-hook 'xslt-process-mode)

(defadvice xml-mode (after run-xml-mode-hooks act)
  "Invoke `xml-mode-hook' hooks in the XML mode."
  (run-hooks 'xml-mode-hook))

;; make xml files writeable, they are not by default, why?
(add-hook 'xml-mode-hook
          (lambda ()
            (toggle-read-only -1)))

(provide 'xslt-process-config)

;;; xslt-process-config.el ends here