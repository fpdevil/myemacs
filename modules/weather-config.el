;;; package  --- weather-config.el
;;;
;;; Commentary:
;;;
;;; Filename.  : weather-config.el
;;; Description: Emacs configuration for getting weather
;;;              it uses the plugin wttrin (wttr.in) for this
;;;
;;; elisp code for getting the real time weather information
;;;===========================================================================
(require 'wttrin)                  ; get weather information
;;;
;;; Code:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provides weather information from wttr.in based on your query condition  ;;
;; command to invoke is wttrin. list any required cities                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq wttrin-default-cities '("Chennai"
                              "Hyderabad"
                              "Chicago"
                              "Houston"))


(provide 'weather-config)

;;; weather-config.el ends here
