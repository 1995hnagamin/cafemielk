;;;
;;; cafemielk.util
;;;

(define-module cafemielk.util
  (use srfi.133)
  (export
   linspace
   vector-linspace
   cross2d
   cross3d
   dot
   vector-unzip2
   )
  )

(select-module cafemielk.util)


(define (linspace min max size)
  (define step (/ (- max min) (- size 1)))
  (map (lambda (i) (+ min (* step i))) (iota size)))

(define (vector-linspace min max size)
  (define step (/ (- max min) (- size 1)))
  (vector-tabulate size (lambda (i) (+ min (* step i)))))

(define (cross2d u v)
  (- (* (vector-ref u 0) (vector-ref v 1))
     (* (vector-ref u 1) (vector-ref v 0))))

(define (cross3d u v)
  (vector (- (* (vector-ref u 1) (vector-ref v 2))
             (* (vector-ref u 2) (vector-ref v 1)))
          (- (* (vector-ref u 2) (vector-ref v 0))
             (* (vector-ref u 0) (vector-ref v 2)))
          (- (* (vector-ref u 0) (vector-ref v 1))
             (* (vector-ref u 1) (vector-ref v 0)))))

(define (dot u v)
  (vector-fold (lambda (acc ui vi) (+ acc (* ui vi))) 0 u v))

(define (vector-unzip2 vector-of-vectors)
  (define N (vector-length vector-of-vectors))
  (define vec1 (make-vector N))
  (define vec2 (make-vector N))
  (let loop ((i 0))
    (cond
     ((= i N)
      (values vec1 vec2))
     (else
      (vector-set! vec1 i (vector-ref (vector-ref vector-of-vectors i) 0))
      (vector-set! vec2 i (vector-ref (vector-ref vector-of-vectors i) 1))
      (loop (+ i 1))))))
