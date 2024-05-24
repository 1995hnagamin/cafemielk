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
  (mesh2d-trinix-for-each
   (lambda (trinix)
     (define trig (mesh2d-trinix->trig Th trinix))
     (define (x_ i) (vector-ref trig i))
     (define (y_ i) (vector-ref trig (+ i 3)))
     (define b (vec3d-tabulate (lambda (i j k) (- (y_ j) (y_ k)))))
     (define c (vec3d-tabulate (lambda (i j k) (- (x_ k) (x_ j)))))
     (let loop ((i 0) (j 0))
       (cond
        ((= i 3) #f)
        ((= j 3) (loop (+ i 1) 0))
        (else
         (hash-table-update!/default
          dok
          (vector (vector-ref trinix i) (vector-ref trinix j))
          (lambda (val)
            (+ val
               (* (+ (* (vector-ref b i) (vector-ref b j))
                     (* (vector-ref c i) (vector-ref c j)))
                  (/ nu (* 4 (trig2d-area trig))))))
          0)
         (loop i (+ j 1))))))
   Th)
  (make-matrix N N dok))
