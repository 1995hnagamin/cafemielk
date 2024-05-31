(use cafemielk)
(use gauche.sequence)
(use gauche.time)

;; \int_\Omega \nu (\grad w)\cdot(\grad A)dS = \int_\Omega w J_0 dS

(define Th (mesh2d-unit-square 10 10))

(define (f x y) (+ (* x x) (* y y)))

(define fel (func->fel Th f))

(define nu 1)

(define j0 1)

(define (make-coeff-matrix Th)
  (define N (mesh2d-nodes-length Th))
  (define dok (make-hash-table 'equal?))
  (mesh2d-trinix-for-each
   (lambda (trinix)
     (define trig (mesh2d-trinix->trig Th trinix))
     (define b
       (vec3d-tab
        (i i+1 i+2)
        (- (trig2d-yref trig i+1) (trig2d-yref trig i+2))))
     (define c
       (vec3d-tab
        (i i+1 i+2)
        (- (trig2d-xref trig i+2) (trig2d-xref trig i+1))))
     (let loop ((i 0) (j 0))
       (cond
        ((= i 3) #f)
        ((= j 3) (loop (+ i 1) 0))
        (else
         (hash-table-update!/default
          dok
          (vector (vector-ref trinix i) (vector-ref trinix j))
          (lambda (val)
            (+. val
                (*. (+. (*. (vector-ref b i) (vector-ref b j))
                        (*. (vector-ref c i) (vector-ref c j)))
                    (/. nu (*. 4 (trig2d-area trig))))))
          0)
         (loop i (+ j 1))))))
   Th)
  (make-matrix N N (make-dok dok)))

(define (make-rhs-vector Th)
  (define N (mesh2d-nodes-length Th))
  (define rhs (make-vector N 0.))
  (mesh2d-trinix-for-each
   (lambda (trinix)
     (let ((JS (*. j0 (trig2d-area (mesh2d-trinix->trig Th trinix)) 1/3)))
       (do ((i 0 (+ i 1)))
           ((= i 3))
         (let ((vi (vector-ref trinix i)))
           (vector-set! rhs vi (+. (vector-ref rhs vi) JS))))))
   Th)
  rhs)

(define A0 0.)

(define (make-equation Th)
  (define K (make-coeff-matrix Th))
  (define b (make-rhs-vector Th))
  (define numnodes (mesh2d-nodes-length Th))
  (mesh2d-nodes-for-each-with-index
   (lambda (k x y)
     (cond
      ((or (= x 0) (= x 1) (= y 0) (= y 1))
       (let loop ((j 0))
         (cond
          ((= j numnodes) #f)
          ((= j k) (loop (+ j 1)))
          (else
           (hash-table-delete! (slot-ref (matrix-data K) 'vals)
                               (vector k j))
           (loop (+ j 1)))))
       (hash-table-set! (slot-ref (matrix-data K) 'vals)
                        (vector k k) 1.)
       (vector-set! b k A0)
       (let loop ((i 0))
         (cond
          ((= i numnodes) #f)
          ((= i k) (loop (+ i 1)))
          (else
           (vector-set!
            b i
            (- (vector-ref b i)
               (*. (matrix-ref K i k) A0)))
           (hash-table-delete! (slot-ref (matrix-data K) 'vals)
                               (vector i k))))))
      (else)))
   Th)
  (values (matrix-coo->csr (matrix-dok->coo K)) b))
