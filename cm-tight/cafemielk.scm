;;;
;;; cafemielk
;;;

(define-module cafemielk
  (extend cafemielk.util
          cafemielk.vview)
  (export
   <coo>
   <csr>
   <mesh2d>
   <rmaj>
   coo-cols
   coo-rows
   coo-vals
   coo->csr
   coom->csrm
   csr-addmv!
   csr-mv
   dokm->coom
   func->fel
   make-coo
   make-csr
   make-matrix
   make-mesh2d
   make-rmaj
   matrix-data
   mesh2d-nodes
   mesh2d-nodes-length
   mesh2d-nodes-ref
   mesh2d-triangles
   mv
   ncols
   nrows
   rmaj-addmv!
   rmaj-mv
   square
   )
  )
(select-module cafemielk)

;; Loads extension
(dynamic-load "cafemielk")

;;
;; Put your Scheme definitions here
;;


;; cafemielk.linalg

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

;; CSR (compressed sparse row)

(define-class <csr> ()
  ((vals :init-keyword :vals)
   (rowptr :init-keyword :rowptr)
   (colind :init-keyword :colind)))

(define (make-csr vals rowptr colind)
  (make <csr>
    :vals vals :rowptr rowptr :colind colind))

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

;; cafemielk.mesh

(use cafemielk.util)
(use cafemielk.vview)

(define-class <mesh2d> ()
  ((nodes :init-keyword :nodes)
   (triangles :init-keyword :triangles)))

(define (make-mesh2d nodes triangles)
  (make <mesh2d> :nodes nodes :triangles triangles))

(define (mesh2d-nodes mesh)
  (slot-ref mesh 'nodes))

(define (mesh2d-triangles mesh)
  (slot-ref mesh 'triangles))

(define (mesh2d-nodes-length mesh)
  (vview-length (mesh2d-nodes mesh) 0))

(define (mesh2d-nodes-ref mesh i)
  (vview-cut (mesh2d-nodes mesh) (vector i)))

(define (square nx ny)
  (define ns (square-point-vec (vector-linspace 0. 1. nx)
                               (vector-linspace 0. 1. ny)))
  (define ts (square-triangle-vec nx ny))
  (make-mesh2d
   (make-vview ns 0 (vector (* nx ny) 2))
   (make-vview ts 0 (vector (* 2 (- nx 1) (- ny 1)) 3))))


;; cafemielk.fel

(define (func->fel Th func)
  (vector-tabulate
   (mesh2d-nodes-length Th)
   (lambda (i)
     (let ((p (mesh2d-nodes-ref Th i)))
       (func (vview-ref p #(0))
             (vview-ref p #(1)))))))
