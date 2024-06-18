(define-module cafemielk.integ
  (use cafemielk.util)
  (export
   int2d
   int2d-smesh
   d/dxi
   d/deta
   detJ
   )
  )

(select-module cafemielk.integ)

(define (quadrature2d weights xs ys f)
  (fold (lambda (w x y acc)
          (+ acc (* w (f x y))))
        0
        weights
        xs
        ys))

(define (int2d f)
  (define r (/ (sqrt 3)))
  (quadrature2d
   '(1 1 1 1)
   (list (- r) (- r) r r)
   (list (- r)    r  r (- r))
   f))

(define (Ns xi eta)
  (vector
   (* 1/4 (- 1 xi) (- 1 eta))
   (* 1/4 (+ 1 xi) (- 1 eta))
   (* 1/4 (+ 1 xi) (+ 1 eta))
   (* 1/4 (- 1 xi) (+ 1 eta))))

(define (d/dxi xi eta)
  (vector
   (* -1/4 (- 1 eta))
   (* +1/4 (- 1 eta))
   (* +1/4 (+ 1 eta))
   (* -1/4 (+ 1 eta))
   ))

(define (d/deta xi eta)
  (vector
   (* -1/4 (- 1 xi))
   (* -1/4 (+ 1 xi))
   (* +1/4 (+ 1 xi))
   (* +1/4 (- 1 xi))
   )
  )

(define (detJ qlat2d xi eta)
  (define xs
    (vector-tabulate 4 (lambda (i) (vector-ref qlat2d (* 2 i)))))
  (define ys
    (vector-tabulate 4 (lambda (i) (vector-ref qlat2d (+ (* 2 i) 1)))))
  (- (* (dot xs (d/dxi  xi eta))
        (dot ys (d/deta xi eta)))
     (* (dot xs (d/deta xi eta))
        (dot ys (d/dxi  xi eta)))))

(define (int2d-smesh f qlat2d)
  (define xs
    (vector-tabulate 4 (lambda (i) (vector-ref qlat2d (* 2 i)))))
  (define ys
    (vector-tabulate 4 (lambda (i) (vector-ref qlat2d (+ (* 2 i) 1)))))
  (define (x xi eta)
    (dot xs (Ns xi eta)))
  (define (y xi eta)
    (dot ys (Ns xi eta)))
  (int2d (lambda (xi eta)
           (* (f (x xi eta) (y xi eta))
              (abs (detJ qlat2d xi eta))))))
