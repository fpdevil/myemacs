;;; c++11-mode.el --- Major mode for editing C++11 code

;; Name: c++11
;; Author: Roy Crihfield
;; Maintainer: Roy
;; Created: 2014-01-10
;; Date: 2014-01-10
;; Version: 20140110
;; Keywords: C++ C++11 cc-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Source at https://bitbucket.org/roysc/cpp11-mode
;;
;; FIXME: CC Mode is designed with C++ support hardcoded into various places
;;        (e.g. cc-fonts.el). There's no clean way to get the same support
;;        in a separate mode. (A possible monkey-patch would be to redefine
;;        or advise `c-major-mode-is')
;;
;; TODO: context-dependent keywords: final, override
;;       support for `>>' template syntax
;;       fix font-lock of identifiers delcared with `auto'
;;

;; Code:

(require 'cc-mode)

(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts)
  (require 'cl)) ;; work around cc-mode bug


(eval-and-compile
  (c-add-language 'c++11-mode 'c++-mode))


;;; Keywords

;; Built-in basic types
(c-lang-defconst c-primitive-type-kwds
  c++11 (append '("char16_t" "char32_t")
                (c-lang-const c-primitive-type-kwds)))

;; Keywords that can prefix normal declarations of identifiers
(c-lang-defconst c-modifier-kwds
  c++11 (append '("thread_local" "noexcept")
                (c-lang-const c-modifier-kwds)))

;; These can occur almost anywhere in types but they don't build a type of
;; themselves.
(c-lang-defconst c-type-modifier-kwds
  c++11 (append '("constexpr")
                (c-lang-const c-type-modifier-kwds)))

;; Keywords that may be followed by a parenthesis expression that doesn't
;; contain type identifiers.
(c-lang-defconst c-paren-nontype-kwds
  c++11 (append '("decltype" "noexcept" "static_assert")
                (c-lang-const c-paren-nontype-kwds)))

;; Keywords for constants.
(c-lang-defconst c-constant-kwds
  c++11 (append '("nullptr")
                (c-lang-const c-constant-kwds)))


;;; Font and syntax

(defcustom c++11-font-lock-extra-types
  (append '("unordered_map" "unordered_multimap"
            "unordered_set" "unordered_multiset"
            "forward_list"
            "tuple"
            "initializer_list")
          c++-font-lock-extra-types)
  (c-make-font-lock-extra-types-blurb "C++11" "c++11-mode"
"For example, a value of (\"string\") means the word \"string\" is treated
as a type name.")
  :type 'c-extra-types-widget
  :group 'c)

(defconst c++11-font-lock-keywords-1 (c-lang-const c-matchers-1 c++11)
  "Minimal font locking for C++ mode.
Fontifies only preprocessor directives (in addition to the syntactic
fontification of strings and comments).")

(defconst c++11-font-lock-keywords-2 (c-lang-const c-matchers-2 c++11)
  "Fast normal font locking for C++ mode.
In addition to `c++11-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, the
user defined types on `c++11-font-lock-extra-types', and the doc comment
styles specified by `c-doc-comment-style'.")

(defconst c++11-font-lock-keywords-3 (c-lang-const c-matchers-3 c++11)
  "Accurate normal font locking for C++ mode.
Like `c++11-font-lock-keywords-2' but detects declarations in a more
accurate way that works in most cases for arbitrary types without the
need for `c++11-font-lock-extra-types'.")

(defvar c++11-font-lock-keywords c++11-font-lock-keywords-3
  "Default expressions to highlight in C++ mode.")

(defun c++11-font-lock-keywords-2 ()
  (c-compose-keywords-list c++11-font-lock-keywords-2))
(defun c++11-font-lock-keywords-3 ()
  (c-compose-keywords-list c++11-font-lock-keywords-3))
(defun c++11-font-lock-keywords ()
  (c-compose-keywords-list c++11-font-lock-keywords))

;;;###autoload
(defvar c++11-mode-syntax-table nil
  "Syntax table used in c++11-mode buffers.")
(or c++11-mode-syntax-table
    (setq c++11-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table c++11))))

(defvar c++11-mode-abbrev-table nil
  "Abbreviation table used in c++11-mode buffers.")
(c-define-abbrev-table 'c++11-mode-abbrev-table
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)))

(defvar c++11-mode-map ()
  "Keymap used in c++11-mode buffers.")
(if c++11-mode-map
    nil
  (setq c++11-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for C++
  (define-key c++11-mode-map "\C-c\C-e" 'c-macro-expand)
  (define-key c++11-mode-map "\C-c:"    'c-scope-operator)
  (define-key c++11-mode-map "<"        'c-electric-lt-gt)
  (define-key c++11-mode-map ">"        'c-electric-lt-gt))

(easy-menu-define c-c++11-menu c++11-mode-map "C++ Mode Commands"
		  (cons "C++" (c-lang-const c-mode-menu c++11)))

;; TODO: context-dependent keywords: final, override

;;;###autoload
(define-derived-mode c++11-mode c++-mode "C++"
  "Major mode for editing C++ code with support for the C++11 standard.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
c++-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `c++-mode-hook'.

Key bindings:
\\{c++-mode-map}"
  (c-initialize-cc-mode t)
  (set-syntax-table c++11-mode-syntax-table)
  (setq local-abbrev-table c++11-mode-abbrev-table
        abbrev-mode t)
  (use-local-map c++11-mode-map)
  (c-init-language-vars c++11-mode)
  (c-common-init 'c++11-mode)
  (easy-menu-add c-c++11-menu)
  (c-run-mode-hooks 'c-mode-common-hook 'c++11-mode-hook)
  (c-update-modeline))

(provide 'c++11-mode)

;;; c++11-mode.el ends here
