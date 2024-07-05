;;;
;;; cafemielk.mesh.qlat2d
;;; 2D Quadrilateral mesh
;;;

(define-module cafemielk.mesh.qlat2d
  (use cafemielk.util)
  (use cafemielk.vview)
  (extend
   cafemielk.geom.qlat2d)
  (export
   <qlmesh2d>
   qlat2d-xref
   qlat2d-yref
   qlmesh2d-unit-square
   qlmesh2d-vertices-length
   qlmesh2d-vertices-ref
   qlmesh2d-vise->qlat
   qlmesh2d-vise-ref
   )
  )

(select-module cafemielk.mesh.qlat2d)

(define-class <qlmesh2d> ()
  ((vertices :init-keyword :vertices)
   (vises :init-keyword :vises)))

(define (make-qlmesh2d vtxs vises)
  (make <qlmesh2d> :vertices vtxs :vises vises))

(define (qlmesh2d-vertices-length mesh)
  (vview-length (slot-ref mesh 'vertices) 0))

(define (qlmesh2d-vises-length mesh)
  (vview-length (slot-ref mesh 'vises) 0))

(define (qlmesh2d-vertices-ref mesh i)
  (vview-cut (slot-ref mesh 'nodes) (vector i)))

(define (qlmesh2d-vise->qlat mesh vise)
  (define (node i)
    (vview->vector (qlmesh2d-nodes-ref mesh (vector-ref vise i))))
  (define (x_ i) (vector-ref (node i) 0))
  (define (y_ i) (vector-ref (node i) 1))
  (vector (x_ 0) (x_ 1) (x_ 2) (x_ 3)
          (y_ 0) (y_ 1) (y_ 2) (y_ 3)))

(define (%grid-mesh xs ys)
  (define nx (vector-length xs))
  (define ny (vector-length ys))
  (define vec (make-vector (* 2 nx ny)))
  (let loop ((i 0) (j 0) (s 0))
    (cond
     ((= j ny) vec)
     ((= i nx) (loop 0 (+ j 1) s))
     (else
      (vector-set! vec s       (vector-ref xs i))
      (vector-set! vec (+ s 1) (vector-ref ys j))
      (loop (+ i 1) j (+ s 2))))))

(define (%grid-vises nx ny)
  (define vec (make-vector (* 4 (- nx 1) (- ny 1))))
  ;; nx*j+i-1     nx*j+i
  ;;     #--------#
  ;;     |        |
  ;;     #--------#
  ;; nx*(j-1)+i-1 nx*(j-1)+i
  (let loop ((i 1) (j 1) (s 0))
    (cond
     ((= j ny) vec)
     ((= i nx) (loop 1 (+ j 1) s))
     (else
      (vector-set! vec s       (+ (* nx (- j 1)) i -1))
      (vector-set! vec (+ s 1) (+ (* nx (- j 1)) i))
      (vector-set! vec (+ s 2) (+ (* nx j) i -1))
      (vector-set! vec (+ s 3) (+ (* nx j) i))
      (loop (+ i 1) j (+ s 4))))))

(define (qlmesh2d-unit-square nx ny)
  (make-qlmesh2d
   (make-vview
    (%grid-mesh (vector-linspace 0. 1. nx)
                (vector-linspace 0. 1. ny))
    0 (vector (* nx ny) 2))
   (make-vview
    (%grid-vises nx ny)
    0 (vector (* (- nx 1) (- ny 1)) 4))))
