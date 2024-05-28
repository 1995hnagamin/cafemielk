;;;
;;; cafemielk.linalg
;;; Linear algebra library for Cafemielk
;;;

(define-module cafemielk.linalg
  (use cafemielk.vview)
  (use gauche.sequence)
  (export
   <csr>
   <coo>
   <csr>
   <rmaj>
   coo-cols
   coo-rows
   coo-vals
   coo->csr
   coom->csrm
   csr-addmv!
   csr-mv
   csr-ref
   dokm->coom
   make-coo
   make-csr
   make-matrix
   make-rmaj
   matrix-data
   matrix-ref
   mv
   ncols
   nrows
   rmaj-addmv!
   rmaj-mv
   )
  )

(select-module cafemielk.linalg)

(define-class <matrix> ()
  ((nrows :init-keyword :nrows)
   (ncols :init-keyword :ncols)
   (data :init-keyword :data)))

(define (make-matrix nr nc data)
  (make <matrix>
    :nrows nr :ncols nc :data data))

(define (nrows matrix) (slot-ref matrix 'nrows))
(define (ncols matrix) (slot-ref matrix 'ncols))
(define (matrix-data matrix) (slot-ref matrix 'data))

(define-method mv ((M <matrix>) v)
  (mv (nrows M) (ncols M) (matrix-data M) v))

(define-method matrix-ref ((M <matrix>) i j)
  (csr-ref (matrix-data M) i j))

;; CSR (compressed sparse row)

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

(define (csr-addmv! nr nc A x y)
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

(define (csr-mv nr nc A x)
  (define y (make-vector nr 0))
  (csr-addmv! nr nc A x y)
  y)

(define-method mv (nr nc (A <csr>) v)
  (csr-mv nr nc A v))

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

;; COO (coordinate format)

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

(define (coom->csrm coom)
  (make-matrix
   (nrows coom)
   (ncols coom)
   (coo->csr (nrows coom) (matrix-data coom))))

;; DOK (dictionary of keys)

(define (dokm->coom dokm)
  (define dok (matrix-data dokm))
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
    (make-matrix
     (nrows dokm)
     (ncols dokm)
     (make-coo vals rows cols))))

;; row-major dense matrix

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
