(use cafemielk :prefix cm:)
(use gauche.sequence)

; \int_\Omega \nu (\grad w)\cdot(\grad A)dS = \int_\Omega w J_0 dS

(define Th (cm:square 5 5))

(define (f x y) (+ (* x x) (* y y)))

(define (cross u v)
  (- (* (vector-ref u 0) (vector-ref v 1))
     (* (vector-ref u 1) (vector-ref v 0))))

(define (dokm->coom dokm)
  (define dok (cm:matrix-data dokm))
  (define nnz (hash-table-size dok))
  (let ((vals (make-vector nnz))
	(rows (make-vector nnz))
	(cols (make-vector nnz)))
    (for-each-with-index
     (lambda (i kv)
       (vector-set! vals i (cdr kv))
       (vector-set! rows i (vector-ref (car kv) 0))
       (vector-set! cols i (vector-ref (car kv) 1)))
     (sort (hash-table->alist dok)))
    (cm:make-matrix
     (cm:nrows dokm)
     (cm:ncols dokm)
     (cm:make-coo vals rows cols))))

(define nu 1)

(define (make-matrix Th)
  (define N (cm:mesh2d-nodes-length Th))
  (define dok (make-hash-table 'equal?))
  (vector-for-each
   (lambda (triangle)
     (let* ((v (vector-map
		(lambda (k)
		  (cm:vview->vector (cm:mesh2d-nodes-ref Th k)))
		triangle))   ; v_i = (x_i, y_i)  [i = 0, 1, 2]
	    (v01 (vector-map - (vector-ref v 1) (vector-ref v 0)))
	    (v02 (vector-map - (vector-ref v 2) (vector-ref v 0)))
	    (S (/ (cross v01 v02) 2))   ; v0, v1, v2 are counter clock-wise
	    (b (vector-tabulate
		3
		(lambda (i)
		  (- (vector-ref (vector-ref v (modulo (+ i 1) 3)) 1)
		     (vector-ref (vector-ref v (modulo (+ i 2) 3)) 1)))))
	    (c (vector-tabulate
		3
		(lambda (i)
		  (- (vector-ref (vector-ref v (modulo (+ i 2) 3)) 0)
		     (vector-ref (vector-ref v (modulo (+ i 1) 3)) 0))))))
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
   (cm:vview-stratify (cm:mesh2d-triangles Th)))
  (cm:make-matrix N N dok))
