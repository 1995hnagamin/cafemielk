;;;
;;; cafemielk.util
;;;

(define-module cafemielk.util
  (use srfi.133)
  (export
   linspace
   vector-linspace
   cross2
   cross3
   dot
   )
  )

(select-module cafemielk.util)


(define (linspace min max size)
  (define step (/ (- max min) (- size 1)))
  (map (lambda (i) (+ min (* step i))) (iota size)))

(define (vector-linspace min max size)
  (define step (/ (- max min) (- size 1)))
  (vector-tabulate size (lambda (i) (+ min (* step i)))))

(define (cross2 u v)
  (- (* (vector-ref u 0) (vector-ref v 1))
     (* (vector-ref u 1) (vector-ref v 0))))

(define (cross3 u v)
  (vector (- (* (vector-ref u 1) (vector-ref v 2))
             (* (vector-ref u 2) (vector-ref v 1)))
          (- (* (vector-ref u 2) (vector-ref v 0))
             (* (vector-ref u 0) (vector-ref v 2)))
          (- (* (vector-ref u 0) (vector-ref v 1))
             (* (vector-ref u 1) (vector-ref v 0)))))

(define (dot u v)
  (vector-fold (lambda (acc ui vi) (+ acc (* ui vi))) 0 u v))
