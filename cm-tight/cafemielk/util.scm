;;;
;;; cafemielk.util
;;;

(define-module cafemielk.util
  (export linspace
         vector-linspace
         )
  )

(select-module cafemielk.util)

(define (linspace min max size)
  (define step (/ (- max min) (- size 1)))
  (map (lambda (i) (+ min (* step i))) (iota size)))

(define (vector-linspace min max size)
  (define step (/ (- max min) (- size 1)))
  (vector-tabulate size (lambda (i) (+ min (* step i)))))
