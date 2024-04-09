;;;
;;; cafemielk
;;;

(define-module cafemielk
  (export test-cafemielk ;; dummy
          *magic-number*
          )
  )
(select-module cafemielk)

;; Loads extension
(dynamic-load "cafemielk")

;;
;; Put your Scheme definitions here
;;

(define *magic-number* 42)
