;;; package --- Hydra and related plugins for Emacs
;;;
;;; Commentary:
;;; Filename: hydra-config.el
;;; description:
;;; Hydra allows me to  group binds together. It also shows  a list of all
;;; implemented commands in the eho area
;;; Once you summon the Hydra through the prefixed binding (the body + any
;;; one head),  all heads can  be called in  succession with only  a short
;;; extension.
;;; The  Hydra is  vanquished once  Hercules, any  binding that  isn’t the
;;; Hydra’s  head, arrives.  Note that  Hercules, besides  vanquishing the
;;; Hydra,  will still  serve  his original  purpose,  calling his  proper
;;; command. This  makes the Hydra very  seamless, it’s like a  minor mode
;;; that disables itself auto-magically.
;;;             -- By Oleh Krehel
;;;
;;; Code:
;;;

(use-package hydra
  :preface
  (defvar-local /hydra/ongoing-hydra-body nil)
  (defun /hydra/ongoing-hydra ()
    (interactive)
    (if /hydra/ongoing-hydra-body
        (funcall /hydra/ongoing-hydra-body)
      (user-error "/hydra/ongoing-hydra: /hydra/ongoing-hydra-body is not set")))
  :bind
  (
   ;;("C-x R t" . hydra-toggle/body)
   ("C-x R d" . hydra-dates/body)
   ("C-x R f" . hydra-flycheck/body)
   ("C-x R g" . hydra-magit/body)
   ("C-x R h" . hydra-helm/body)
   ("C-x R o" . /hydra/ongoing-hydra)
   ("C-x R j" . hydra-projectile/body)
   ("C-x R s" . hydra-system/body)
   ("C-x R w" . hydra-windows/body)
   ("C-x R m" . hydra-multiple-cursors/body))
  :config (setq-default hydra-default-hint nil))


;;** Hydra / Toggle
;; Toggle options through Hydra
(defhydra hydra-toggle (:color pink
                          :hint nil)
    "
_a_: abbrev-mode
_d_: debug-on-error
_f_: auto-fill-mode
_o_: org-mode
_t_: truncate-lines
_w_: whitespace-mode
_q_: Quit
    "
    ("a" abbrev-mode nil)
    ("d" toggle-debug-on-error nil)
    ("f" auto-fill-mode nil)
    ("o" org-mode nil)
    ("t" toggle-truncate-lines nil)
    ("w" whitespace-mode nil)
    ("q" nil)
    )


;;** Hydra / Dates
;; group all the date-related commands
(defhydra hydra-dates (:color blue)
  "
^
^Dates^             ^Insert^            ^Insert with Time^
^─────^─────────────^──────^────────────^────────────────^──
_q_ quit            _d_ short           _D_ short
^^                  _i_ iso             _I_ iso
^^                  _l_ long            _L_ long
^^                  ^^                  ^^
"
  ("q" nil)
  ("d" me/date-short)
  ("D" me/date-short-with-time)
  ("i" me/date-iso)
  ("I" me/date-iso-with-time)
  ("l" me/date-long)
  ("L" me/date-long-with-time))


;;** Hydra / FlyCheck
;; group hydra-flycheck commands
(defhydra hydra-flycheck (:color pink)
  "
^
^Flycheck^          ^Errors^            ^Checker^
^────────^──────────^──────^────────────^───────^───────────
_q_ quit            _<_ previous        _?_ describe
_m_ manual          _>_ next            _d_ disable
_v_ verify setup    _f_ check           _s_ select
^^                  _l_ list            _h_ helm flycheck
^^                  ^^                  ^^
"
  ("q" nil)
  ("<" flycheck-previous-error)
  (">" flycheck-next-error)
  ("?" flycheck-describe-checker :color blue)
  ("d" flycheck-disable-checker :color blue)
  ("f" flycheck-buffer)
  ("h" helm-flycheck :color green)
  ("l" flycheck-list-errors :color blue)
  ("m" flycheck-manual :color blue)
  ("s" flycheck-select-checker :color blue)
  ("v" flycheck-verify-setup :color blue))


;;** Hydra / Magit
;; group magit commands
(defhydra hydra-magit (:color blue)
  "
^
^Magit^             ^Do^
^─────^─────────────^──^────────────────
_q_ quit            _b_ blame
^^                  _c_ clone
^^                  _i_ init
^^                  _s_ status
^^                  ^^
"
  ("q" nil)
  ("b" magit-blame)
  ("c" magit-clone)
  ("i" magit-init)
  ("s" magit-status))

;;** Hydra / Markdown
;; group Markdownown commands
(defhydra hydra-markdown (:color pink)
  "
^
^Markdown^          ^Table Columns^     ^Table Rows^
^────────^──────────^─────────────^─────^──────────^────────
_q_ quit            _c_ insert          _r_ insert
^^                  _C_ delete          _R_ delete
^^                  _M-<left>_ left     _M-<down>_ down
^^                  _M-<right>_ right   _M-<up>_ up
^^                  ^^                  ^^
"
  ("q" nil)
  ("c" markdown-table-insert-column)
  ("C" markdown-table-delete-column)
  ("r" markdown-table-insert-row)
  ("R" markdown-table-delete-row)
  ("M-<left>" markdown-table-move-column-left)
  ("M-<right>" markdown-table-move-column-right)
  ("M-<down>" markdown-table-move-row-down)
  ("M-<up>" markdown-table-move-row-up))


;;** hydra / Org
;; group Org mode commands
(defhydra hydra-org (:color pink)
  "
^
^Org^               ^Links^             ^Outline^
^───^───────────────^─────^─────────────^───────^───────────
_q_ quit            _i_ insert          _<_ previous
^^                  _n_ next            _>_ next
^^                  _p_ previous        _a_ all
^^                  _s_ store           _o_ goto
^^                  ^^                  _v_ overview
^^                  ^^                  ^^
"
  ("q" nil)
  ("<" org-backward-element)
  (">" org-forward-element)
  ("a" outline-show-all)
  ("i" org-insert-link :color blue)
  ("n" org-next-link)
  ("o" helm-org-in-buffer-headings :color blue)
  ("p" org-previous-link)
  ("s" org-store-link)
  ("v" org-overview))


;;** Hydra / Projectile
;; group projectile commands
(defhydra hydra-projectile (:color blue)
  "
^
^Projectile^        ^Buffers^           ^Find^              ^Search^
^──────────^────────^───────^───────────^────^──────────────^──────^────────────
_q_ quit            _b_ list            _d_ directory       _r_ replace
_i_ reset cache     _K_ kill all        _D_ root            _R_ regexp replace
^^                  _S_ save all        _f_ file            _s_ ag
^^                  ^^                  _p_ project         ^^
^^                  ^^                  ^^                  ^^
"
  ("q" nil)
  ("b" helm-projectile-switch-to-buffer)
  ("d" helm-projectile-find-dir)
  ("D" projectile-dired)
  ("f" helm-projectile-find-file)
  ("i" projectile-invalidate-cache :color red)
  ("K" projectile-kill-buffers)
  ("p" helm-projectile-switch-project)
  ("r" projectile-replace)
  ("R" projectile-replace-regexp)
  ("s" helm-projectile-ag)
  ("S" projectile-save-project-buffers))


;;** Hydra / ReactJS RJSX
;; group react js commands
(defhydra hydra-rjsx (:color pink)
  "
^
^RJSX^
^────^──────────────
_q_ quit
^^
"
  ("q" nil))


;;** Hydra / System
;; group system related commands
(defhydra hydra-system (:color blue)
  "
^
^System^            ^Packages^          ^Processes^
^──────^────────────^────────^──────────^─────────^─────────
_q_ quit            _p_ list            _s_ list
^^                  _P_ upgrade         ^^
^^                  ^^                  ^^
"
  ("q" nil)
  ("p" paradox-list-packages)
  ("P" paradox-upgrade-packages)
  ("s" list-processes))


;;** Hydra / Windows
;; group window related commands
(defhydra hydra-windows (:color pink)
  "
^
^Windows^           ^Window^            ^Zoom^
^───────^───────────^──────^────────────^────^──────────────
_q_ quit            _b_ balance         _-_ out
^^                  _i_ heighten        _+_ in
^^                  _j_ narrow          _=_ reset
^^                  _k_ lower           ^^
^^                  _l_ widen           ^^
^^                  ^^                  ^^
"
  ("q" nil)
  ("b" balance-windows)
  ("i" enlarge-window)
  ("j" shrink-window-horizontally)
  ("k" shrink-window)
  ("l" enlarge-window-horizontally)
  ("-" text-scale-decrease)
  ("+" text-scale-increase)
  ("=" (text-scale-increase 0)))


;;** Hydra / Multiple cursors
;; group multiple cursor options through hydra
  (defhydra hydra-multiple-cursors (:color pink
                                    :hint nil)
    "
              -- MULTIPLE CURSORS MENU --
^^^^^^^^---------------------------------------------------------------------------
^Next^                   ^Previous^                  ^Others^
^^^^^^^^---------------------------------------------------------------------------
_n_:   Mark Next Line    _p_:   Mark Previous Line   _a_: Mark All Lines
_N_:   Skip Next Line    _P_:   Skip Previous Line   _l_: Edit Lines
_M-n_: Unmark Next Line  _M-p_: Unmark Previous Line _r_: Mark by regexp
_q_:   Quit
    "
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("a" mc/mark-all-like-this :exit t)
    ("l" mc/edit-lines :exit t)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("q" nil)
    )


;;** date helping functions...
(defun me/date-iso ()
  "Insert the current date, ISO format, eg. 2016-12-09."
  (interactive)
  (insert (format-time-string "%F")))

(defun me/date-iso-with-time ()
  "Insert the current date, ISO format with time, eg. 2016-12-09T14:34:54+0100."
  (interactive)
  (insert (format-time-string "%FT%T%z")))

(defun me/date-long ()
  "Insert the current date, long format, eg. December 09, 2016."
  (interactive)
  (insert (format-time-string "%B %d, %Y")))

(defun me/date-long-with-time ()
  "Insert the current date, long format, eg. December 09, 2016 - 14:34."
  (interactive)
  (insert (capitalize (format-time-string "%B %d, %Y - %H:%M"))))

(defun me/date-short ()
  "Insert the current date, short format, eg. 2016.12.09."
  (interactive)
  (insert (format-time-string "%Y.%m.%d")))

(defun me/date-short-with-time ()
  "Insert the current date, short format with time, eg. 2016.12.09 14:34"
  (interactive)
  (insert (format-time-string "%Y.%m.%d %H:%M")))


(provide 'hydra-config)

;;; hydra-config.el ends here
