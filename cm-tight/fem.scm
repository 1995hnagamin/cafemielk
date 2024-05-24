(use cafemielk)
(use gauche.sequence)

;; \int_\Omega \nu (\grad w)\cdot(\grad A)dS = \int_\Omega w J_0 dS

(define Th (mesh2d-unit-square 5 5))

(define (f x y) (+ (* x x) (* y y)))

(define fel (func->fel Th f))

(define nu 1)

(define (make-coeff-matrix Th)
  (define N (mesh2d-nodes-length Th))
  (define dok (make-hash-table 'equal?))
  (vector-for-each
   (lambda (triangle)
     (define v (vector-map
                (lambda (k)
                  (vview->vector (mesh2d-nodes-ref Th k)))
                triangle)) ; v_i = (x_i, y_i)  [i = 0, 1, 2]
     (define (v_ i) (vector-ref v i))
     (define v01 (vector-map - (v_ 1) (v_ 0)))
     (define v02 (vector-map - (v_ 2) (v_ 0)))
     (define S (* 1/2 (cross2d v01 v02))) ; v0, v1, v2 are counter clockwise
     (define b (vec3d-tabulate
                (lambda (i j k)
                  (- (vector-ref (v_ j) 1)
                     (vector-ref (v_ k) 1)))))
     (define c (vec3d-tabulate
                (lambda (i j k)
                  (- (vector-ref (v_ k) 0)
                     (vector-ref (v_ j) 0)))))
     (vector-for-each-with-index
      (lambda (i vi)
        (vector-for-each-with-index
         (lambda (j vj)
           (hash-table-update!/default
            dok
            (vector (vector-ref triangle i) (vector-ref triangle j))
            (lambda (val)
              (+ val
                 (* (+ (* (vector-ref b i) (vector-ref b j))
                       (* (vector-ref c i) (vector-ref c j)))
                    (/ nu (* 4 S)))))
            0))
         v))
      v))
   (vview-stratify (mesh2d-triangles Th)))
  (make-matrix N N dok))
