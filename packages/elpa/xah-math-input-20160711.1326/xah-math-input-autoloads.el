;;; xah-math-input-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "xah-math-input" "xah-math-input.el" (22536
;;;;;;  14340 0 0))
;;; Generated autoloads from xah-math-input.el

(defvar xah-math-input-mode nil "\
Non-nil if Xah-Math-Input mode is enabled.
See the `xah-math-input-mode' command
for a description of this minor mode.")

(custom-autoload 'xah-math-input-mode "xah-math-input" nil)

(autoload 'xah-math-input-mode "xah-math-input" "\
Toggle xah-math-input minor mode.

A mode for inputting a math and Unicode symbols.

Type “inf”, then press \\[xah-math-input-change-to-symbol], then it becomes “∞”.

Other examples:
 a ⇒ α
 p ⇒ π
 /= ⇒ ≠
 >= ⇒ ≥
 => ⇒ ⇒
 -> ⇒ →
 and ⇒ ∧
etc.

If you have a text selection, then selected word will be taken as input. For example, type 「extraterrestrial alien」, select the phrase, then press 【Shift+Space】, then it becomse 「👽」.

For the complete list of abbrevs, call `xah-math-input-list-math-symbols'.

Decimal and hexadecimal can also be used. Example:

 945     ← decimal
 #945    ← decimal with prefix #
 &#945;  ← XML entity syntax

 x3b1    ← hexadimal with prefix x
 #x3b1   ← hexadimal with prefix #x
 &#x3b1; ← XML entity syntax

Full Unicode name can also be used, e.g. 「greek small letter alpha」.

If you wish to enter a symbor by full unicode name but do not
know the full name, call `ucs-insert'. Asterisk “*” can be used
as a wildcard to find the char. For example, call
“ucs-insert”, then type 「*arrow」 then Tab, then emacs will list
all unicode char names that has “arrow” in it. (this feature is
part of Emacs 23)

Home page at: URL `http://ergoemacs.org/emacs/xah-math-input-math-symbols-input.html'

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; xah-math-input-autoloads.el ends here
