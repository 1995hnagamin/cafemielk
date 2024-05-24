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
     (let* ((v (vector-map
                (lambda (k)
                  (vview->vector (mesh2d-nodes-ref Th k)))
                triangle))   ; v_i = (x_i, y_i)  [i = 0, 1, 2]
            (v01 (vector-map - (vector-ref v 1) (vector-ref v 0)))
            (v02 (vector-map - (vector-ref v 2) (vector-ref v 0)))
            (S (/ (cross2d v01 v02) 2))   ; v0, v1, v2 are counter clock-wise
            (b (vec3d-tabulate
                (lambda (i j k)
                  (- (vector-ref (vector-ref v j) 1)
                     (vector-ref (vector-ref v k) 1)))))
            (c (vec3d-tabulate
                (lambda (i j k)
                  (- (vector-ref (vector-ref v k) 0)
                     (vector-ref (vector-ref v j) 0))))))
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
        v)))
   (vview-stratify (mesh2d-triangles Th)))
  (make-matrix N N dok))
