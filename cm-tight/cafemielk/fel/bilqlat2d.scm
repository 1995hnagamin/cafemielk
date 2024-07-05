;;;
;;; cafemielk.bilqlat2d
;;; Bilinear quadrilateral elements
;;;

(define-module cafemielk.fel.bilqlat2d
  (use cafemielk.mesh.qlat2d)
  (use cafemielk.util)
  (use srfi.133)
  (export
   int2d
   int2d-smesh
   d/dxi
   d/deta
   detJ
   )
  )

(select-module cafemielk.fel.bilqlat2d)


(define-inline (quadrature2d weights xs ys f)
  (vector-fold (lambda (acc w x y)
                 (+ acc (* w (f x y))))
               0 weights xs ys))

(define-inline (int2d f)
  (define t (/ (sqrt 3)))
  (quadrature2d
   #(1 1 1 1)
   (vector (- t) (- t)    t     t)
   (vector (- t)    t     t  (- t))
   f))

(define-inline (Ns xi eta)
  (vector
   (* 1/4 (- 1 xi) (- 1 eta))
   (* 1/4 (+ 1 xi) (- 1 eta))
   (* 1/4 (+ 1 xi) (+ 1 eta))
   (* 1/4 (- 1 xi) (+ 1 eta))))

(define-inline (d/dxi xi eta)
  (vector
   (* -1/4 (- 1 eta))
   (* +1/4 (- 1 eta))
   (* +1/4 (+ 1 eta))
   (* -1/4 (+ 1 eta))))

(define-inline (d/deta xi eta)
  (vector
   (* -1/4 (- 1 xi))
   (* -1/4 (+ 1 xi))
   (* +1/4 (+ 1 xi))
   (* +1/4 (- 1 xi))))

(define-inline (detJ qlat2d xi eta)
  (define xs
    (vector-tabulate 4 (lambda (i) (vector-ref qlat2d i))))
  (define ys
    (vector-tabulate 4 (lambda (i) (vector-ref qlat2d (+ i 4)))))
  (- (* (dot xs (d/dxi  xi eta))
        (dot ys (d/deta xi eta)))
     (* (dot xs (d/deta xi eta))
        (dot ys (d/dxi  xi eta)))))

(define (int2d-smesh f qlat2d)
  (define xs
    (vector-tabulate 4 (lambda (i) (vector-ref qlat2d i))))
  (define ys
    (vector-tabulate 4 (lambda (i) (vector-ref qlat2d (+ i 4)))))
  (define (x xi eta)
    (dot xs (Ns xi eta)))
  (define (y xi eta)
    (dot ys (Ns xi eta)))
  (int2d (lambda (xi eta)
           (* (f (x xi eta) (y xi eta))
              (abs (detJ qlat2d xi eta))))))
