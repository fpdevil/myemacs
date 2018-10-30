;;; package  --- weather-config.el
;;;
;;; Commentary:
;;;
;;; Filename.  : weather-config.el
;;; Description: Emacs configuration for getting weather
;;;              it uses the plugin wttrin (wttr.in) for this
;;;              and sunshine Emacs plugin
;;;
;;; elisp code for getting the real time weather information
;;;
;;; Code:
;;;
;;;===========================================================================
(require 'wttrin)                  ; get weather information
(require 'sunshine)                ; weather and forecast information

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provides weather information from wttr.in based on your query condition  ;;
;; command to invoke is wttrin. list any required cities                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq wttrin-default-cities '("Chennai"
                              "Hyderabad"
                              "Chicago"
                              "Houston"))

;; sunshine customizations
(setq sunshine-appid "e8ec8a2a343baa242eae803d07568d46")
(setq sunshine-show-icons t)
(setq sunshine-location "60106,USA")
(setq wttrin-default-accept-language '("Accept-Language" . "en-US"))


(provide 'weather-config)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; weather-config.el ends here
