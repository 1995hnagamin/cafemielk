;;;
;;; cafemielk.linalg
;;; Linear algebra library for Cafemielk
;;;

(define-module cafemielk.linalg
  (use cafemielk.util)
  (use cafemielk.vview)
  (use gauche.dictionary)
  (use gauche.sequence)
  (use scheme.vector)
  (export
   <csr>
   <coo>
   <csr>
   <rmaj>
   cg-solve
   coo-cols
   coo-rows
   coo-vals
   coo->csr
   create-rvd
   csr-addmv!
   csr-mv
   csr-ref
   make-coo
   make-csr
   make-diag-precond
   make-dok
   make-rvd
   make-matrix
   make-rmaj
   matrix-coo->csr
   matrix-data
   matrix-dok->coo
   matrix-rvd->csr
   matrix-ncols
   matrix-nrows
   matrix-ref
   mv
   pcg-solve
   rmaj-addmv!
   rmaj-mv
   rvd-delete!
   rvd-ref
   rvd-set!
   rvddata-ref
   rvddata-set!
   ucoo-sort!
   )
  )

(select-module cafemielk.linalg)

;;;
;;; Vector operations
;;;

(define-inline (vector-rescale! y c)
  ;; y *= c
  (define N (vector-length y))
  (do ((i 0 (+ i 1)))
      ((= i N) y)
    (vector-set! y i (* c (vector-ref y i)))))

(define-inline (vector-addv! y x)
  (define N (vector-length y))
  (do ((i 0 (+ i 1)))
      ((= i N) y)
    (vector-set! y i
                 (+ (vector-ref y i)
                    (vector-ref x i)))))

(define-inline (vector-rescale-addv! y c x)
  ;; y := c * y + x
  (vector-rescale! y c)
  (vector-addv! y x))

(define-inline (vector-addcv! y c x)
  ;; y += c * x
  (define N (vector-length y))
  (do ((i 0 (+ i 1)))
      ((= i N) y)
    (vector-set! y i
                 (+ (vector-ref y i)
                    (* c (vector-ref x i))))))

;;;
;;; <matrix> -- General matrix
;;;

(define-class <matrix> ()
  ((nrows :init-keyword :nrows)
   (ncols :init-keyword :ncols)
   (data :init-keyword :data)))

(define (make-matrix nr nc data)
  (make <matrix>
    :nrows nr :ncols nc :data data))

(define (matrix-nrows matrix)
  (slot-ref matrix 'nrows))

(define (matrix-ncols matrix)
  (slot-ref matrix 'ncols))

(define (matrix-data matrix)
  (slot-ref matrix 'data))

(define-method mv ((M <matrix>) v)
  (mv (matrix-nrows M) (matrix-ncols M)
      (matrix-data M) v))

(define-method mv-set! (y (M <matrix>) v)
  (mv-set! y
           (matrix-nrows M) (matrix-ncols M)
           (matrix-data M) v))

(define-method matrix-ref ((M <matrix>) i j)
  (matrix-ref (matrix-data M) i j))

;;;
;;; CSR (compressed sparse row)
;;;

(define-class <csr> ()
  ((vals :init-keyword :vals)
   (rowptr :init-keyword :rowptr)
   (colind :init-keyword :colind)))

(define (make-csr vals rowptr colind)
  (make <csr>
    :vals vals :rowptr rowptr :colind colind))

(define (csr-rowptr-ref A k)
  (vector-ref (slot-ref A 'rowptr) k))

(define (csr-colind-ref A k)
  (vector-ref (slot-ref A 'colind) k))

(define (csr-value-ref A k)
  (vector-ref (slot-ref A 'vals) k))

(define (csr-addmv! y nr nc A x)
  ;; y += A*x
  (do ((i 0 (+ i 1)))
      ((= i nr) #f)
    (do ((j (vector-ref (slot-ref A 'rowptr) i) (+ j 1)))
        ((= j (vector-ref (slot-ref A 'rowptr) (+ i 1))) #f)
      (vector-set!
       y i
       (+ (vector-ref y i)
          (* (vector-ref (slot-ref A 'vals) j)
             (vector-ref x (vector-ref (slot-ref A 'colind) j))))))))

(define (csr-mv-set! y nr nc A x)
  (do ((i 0 (+ i 1)))
      ((= i nr) #f)
    (vector-set! y i 0))
  (csr-addmv! y nr nc A x)
  y)

(define (csr-mv nr nc A x)
  (define y (make-vector nr 0))
  (csr-addmv! y nr nc A x)
  y)

(define-method mv (nr nc (A <csr>) v)
  (csr-mv nr nc A v))

(define-method mv-set! (y nr nc (A <csr>) v)
  (csr-mv-set! y nr nc A v))

(define (csr-ref A i j)
  (let ((start (csr-rowptr-ref A i))
        (end (csr-rowptr-ref A (+ i 1))))
    (let loop ((t start))
      (cond
       ((= t end) 0)
       ((= (csr-colind-ref A t) j)
        (csr-value-ref A t))
       (else
        (loop (+ t 1)))))))

(define-method matrix-ref ((A <csr>) i j)
  (csr-ref A i j))

;;;
;;; COO (coordinate format)
;;;

(define-class <coo> ()
  ((vals :init-keyword :vals)
   (rows :init-keyword :rows)
   (cols :init-keyword :cols)))

(define (make-coo vals rows cols)
  (make <coo> :vals vals :rows rows :cols cols))

(define (coo-vals coo) (slot-ref coo 'vals))
(define (coo-rows coo) (slot-ref coo 'rows))
(define (coo-cols coo) (slot-ref coo 'cols))
(define (coo-nnz coo) (vector-length (coo-vals coo)))

(define (coo->csr nr coo)
  (define rowptr (make-vector (+ nr 1)))
  (define nnz (vector-length (coo-rows coo)))
  (make-csr
   (vector-copy (coo-vals coo))
   (let loop ((i 0) (r -1))
     (cond
      ((= r nr)
       rowptr)
      ((= i nnz)
       (vector-set! rowptr (+ r 1) nnz)
       (loop nnz (+ r 1)))
      ((> (vector-ref (coo-rows coo) i) r)
       (vector-set! rowptr (+ r 1) i)
       (loop i (+ r 1)))
      (else
       (loop (+ i 1) r))))
   (vector-copy (coo-cols coo))))

(define (matrix-coo->csr coom)
  (make-matrix
   (matrix-nrows coom)
   (matrix-ncols coom)
   (coo->csr (matrix-nrows coom) (matrix-data coom))))

;; Sort unsorted COO data.
(define (ucoo-sort! vals rows cols)
  (define N (vector-length vals))
  (let run ((gap N))
    (let loop ((i 0) (i+gap gap) (swapped #f))
      (cond
       ((= i+gap N)
        (if (or (> gap 1) swapped)
            (run (cond
                  ((= gap 1) 1)
                  ((< 11 gap 15) 11)
                  (else (floor->exact (/ gap 1.3)))))
            ;; return otherwise
            ))
       ((or (> (vector-ref rows i) (vector-ref rows i+gap))
            (and (= (vector-ref rows i) (vector-ref rows i+gap))
                 (> (vector-ref cols i) (vector-ref cols i+gap))))
        (vector-swap! vals i i+gap)
        (vector-swap! rows i i+gap)
        (vector-swap! cols i i+gap)
        (loop (+ i 1) (+ i+gap 1) #t))
       (else
        (loop (+ i 1) (+ i+gap 1) swapped))))))

;;;
;;; DOK (dictionary of keys)
;;;

(define-class <dok> () ((vals :init-keyword :vals)))

(define (make-dok dict)
  (make <dok> :vals dict))

(define (dok->coo dok)
  (let* ((nnz (hash-table-size (slot-ref dok 'vals)))
         (vals (make-vector nnz))
         (rows (make-vector nnz))
         (cols (make-vector nnz)))
    (dict-fold (slot-ref dok 'vals)
               (lambda (ij v t)
                 (vector-set! rows t (vector-ref ij 0))
                 (vector-set! cols t (vector-ref ij 1))
                 (vector-set! vals t v)
                 (+ t 1))
               0)
    (ucoo-sort! vals rows cols)
    (make-coo vals rows cols)))

(define (matrix-dok->coo dokm)
  (make-matrix
   (matrix-nrows dokm)
   (matrix-ncols dokm)
   (dok->coo (matrix-data dokm))))

(define-method matrix-ref ((A <dok>) i j)
  (hash-table-get (slot-ref A 'vals) (vector i j) 0.))


;;;
;;; Row-oriented vector of dictionaries
;;;

(define-class <rvd> ()
  ((rows :init-keyword :rows)))

(define-inline (make-rvd rvd)
  (make <rvd> :rows rvd))

(define-inline (create-rvd nr)
  (vector-tabulate nr (lambda (_) (make-hash-table 'eqv?))))

(define-method matrix-ref ((A <rvd>) i j)
  (rvddata-ref (slot-ref A 'rows) i j))

(define-inline (rvddata-ref rvd i j)
  (hash-table-ref/default (vector-ref rvd i) j 0))

(define-inline (rvddata-delete! rvd i j)
  (hash-table-delete! (vector-ref rvd i) j))

(define-inline (rvddata-set! rvd i j val)
  (hash-table-set! (vector-ref rvd i) j val))

(define-inline (rvd-ref A i j)
  (rvddata-ref (slot-ref A 'rows) i j))

(define-inline (rvd-delete! A i j)
  (rvddata-delete! (slot-ref A 'rows) i j))

(define-inline (rvd-set! A i j val)
  (rvddata-set! (slot-ref A 'rows) i j val))

(define (rvd-sort! keys vals)
  (define N (vector-length keys))
  (let run ((gap N))
    (let loop ((i 0) (i+gap gap) (swapped #f))
      (cond
       ((= i+gap N)
        (if (or (> gap 1) swapped)
            (run (cond
                  ((= gap 1) 1)
                  ((< 11 gap 15) 11)
                  (else (floor->exact (/ gap 1.3)))))
            ;; return otherwise
            ))
       ((> (vector-ref keys i) (vector-ref keys i+gap))
        (vector-swap! keys i i+gap)
        (vector-swap! vals i i+gap)
        (loop (+ i 1) (+ i+gap 1) #t))
       (else
        (loop (+ i 1) (+ i+gap 1) swapped))))))

(define (rvd-row-kv rvd i)
  (define ht (vector-ref rvd i))
  (define N (hash-table-size ht))
  (define keys (make-vector N))
  (define vals (make-vector N))
  (hash-table-fold
   (vector-ref rvd i)
   (lambda (k v t)
     (vector-set! keys t k)
     (vector-set! vals t v)
     (+ t 1))
   0)
  (rvd-sort! keys vals)
  (values keys vals))

(define (rvd-size rvd)
  (fold (lambda (ht acc) (+ (hash-table-size ht) acc))
        0 rvd))

(define (rvd->csr rvd)
  (define nr (vector-length rvd))
  (define size (rvd-size rvd))
  (define vals (make-vector size))
  (define rowptr (make-vector (+ nr 1)))
  (define colind (make-vector size))
  (let loop ((i 0) (gt 0))
    (cond
     ((= i nr)
      (vector-set! rowptr nr gt)
      (make-csr vals rowptr colind))
     (else
      (vector-set! rowptr i gt)
      (let-values (((Ni) (hash-table-size (vector-ref rvd i)))
                   ((ks vs) (rvd-row-kv rvd i)))
        (do ((j 0 (+ j 1))
             (t gt (+ t 1)))
            ((= j Ni) (loop (+ i 1) t))
          (vector-set! vals t (vector-ref vs j))
          (vector-set! colind t (vector-ref ks j))))))))

(define (matrix-rvd->csr M)
  (make-matrix
   (matrix-nrows M)
   (matrix-ncols M)
   (rvd->csr (slot-ref (matrix-data M) 'rows))))

;;;
;;; Row-major dense matrix
;;;

(define-class <rmaj> () ((vals :init-keyword :vals)))

(define (make-rmaj vals)
  (make <rmaj> :vals vals))

(define (rmaj-addmv! nr nc A x y)
  ;; y += A*x
  (do ((i 0 (+ i 1))) ((= i nr) #f)
    (do ((j 0 (+ j 1))) ((= j nc) #f)
      (let ((ij (+ (* i nr) j)))
        (vector-set!
         y i
         (+ (vector-ref y i)
            (* (vector-ref A ij) (vector-ref x j))))))))

(define (rmaj-mv nr nc A x)
  (define y (make-vector nr 0))
  (rmaj-addmv! nr nc A x y)
  y)

(define-method mv (nr nc (A <rmaj>) v)
  (rmaj-mv nr nc A v))

;;;
;;; Solvers
;;;

;; Conjugate gradient method
;; (Saad 2003, 199-200)
(define (cg-solve A b :key eps (init-guess #f) (max-iter +inf.0) (debug #f))
  (define threshold^2 (expt (* eps (sqrt (dot b b))) 2))
  (define x (or init-guess
                (make-vector (vector-length b) 0.)))
  (define r (vector-map - b (mv A x)))
  (define p (vector-copy r))
  (define Ap (mv A p))
  (let loop ((iter 0)
             (r^2 (dot r r)))
    (cond
     ((>= iter max-iter)
      (if debug (print "max-iter"))
      (values x #f))
     ((< r^2 threshold^2)
      (if debug (print "converged"))
      (values x #t))
     (else
      (let ((alpha (/ r^2 (dot p Ap))))
        (vector-addcv! x alpha p)
        (vector-addcv! r (- alpha) Ap)
        (let* ((r~^2 (dot r r))
               (beta (/ r~^2 r^2)))
          (vector-rescale-addv! p beta r)
          (mv-set! Ap A p)
          (loop (+ iter 1) r~^2)))))))

;; Preconditioned conjugate gradient method
;; (Saad 2003, 277)
(define (pcg-solve A b
                   :key precond! eps
                   (init-guess #f) (max-iter +inf.0) (debug #f))
  (define threshold^2 (expt (* eps (sqrt (dot b b))) 2))
  (define x (or init-guess
                (make-vector (vector-length b) 0.)))
  (define r (vector-map - b (mv A x)))
  (define z (precond! (make-vector (vector-length r)) r))
  (define p (vector-copy z))
  (define Ap (mv A p))
  (let loop ((iter 0)
             (r^2 (dot r r))
             (r.z (dot r z)))
    (cond
     ((>= iter max-iter)
      (if debug (print "max-iter"))
      (values x #f))
     ((< r^2 threshold^2)
      (if debug (print "converged"))
      (values x #t))
     (else
      (let ((alpha (/ r.z (dot p Ap))))
        (vector-addcv! x alpha p)
        (vector-addcv! r (- alpha) Ap)
        (precond! z r)
        (let* ((r~.z~ (dot r z))
               (beta (/ r~.z~ r.z)))
          (vector-rescale-addv! p beta z)
          (mv-set! Ap A p)
          (loop (+ iter 1) (dot r r) r~.z~)))))))

;; Diganoal scaling
(define (make-diag-precond A)
  (let* ((N (matrix-nrows A))
         (diags (vector-tabulate
                 N
                 (lambda (i) (matrix-ref A i i)))))
    (lambda (dst src)
      (let loop ((i 0))
        (cond
         ((= i N) dst)
         (else
          (vector-set! dst i (/ (vector-ref src i)
                                (vector-ref diags i)))
          (loop (+ i 1))))))))


;;;
;;; References
;;;
;;; Saad, Yousef. 2003.
;;;   Iterative Methods for Sparse Linear Systems. 2nd ed.
;;;   Society for Industrial and Applied Mathematics.
;;;   doi: 10.1137/1.9780898718003
;;;
