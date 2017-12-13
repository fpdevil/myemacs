;;; pydoc-info-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "pydoc-info" "pydoc-info.el" (23063 38550 553207
;;;;;;  564000))
;;; Generated autoloads from pydoc-info.el

(require 'info-look)

(autoload 'pydoc-info-add-help "pydoc-info" "\
Add help specifications for a list of Info FILES.

The added specifications are tailored for use with Info files
generated from Sphinx documents.

MORE-SPECS are additional or overriding values passed to
`info-lookup-add-help'.

\(fn FILES &rest MORE-SPECS)" nil nil)

(pydoc-info-add-help '("python"))

;;;***

;;;### (autoloads nil nil ("pydoc-info-pkg.el") (23063 38550 549646
;;;;;;  811000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; pydoc-info-autoloads.el ends here
