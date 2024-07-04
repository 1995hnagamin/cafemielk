;;;
;;; cafemielk.mesh.qlat
;;; Quadrilateral mesh
;;;

(define-module cafemielk.mesh.qlat
  (export
   <qlmesh2d>
   qlat2d-xref
   qlat2d-yref
   qlmesh2d-vertices-ref
   qlmesh2d-vise->qlat
   qlmesh2d-vise-ref
   )
  )

(select-module cafemielk.mesh.qlat)

(define-inline (qlat2d-xref qlat i)
  (vector-ref qlat i))

(define-inline (qlat2d-yref qlat i)
  (vector-ref qlat (+ i 4)))

(define-class <qlmesh2d> ()
  ((vertices :init-keyword :vertices)
   (vises :init-keyword :vises)))

(define (make-qlmesh2d vtxs vises)
  (make <qlmesh2d> :vertices vtxs :vises vises))

(define (qlmesh2d-vises-length mesh)
  (vview-length (slot-ref 'vises)))

(define (qlmesh2d-vertices-ref mesh i)
  (vview-cut (slot-ref mesh 'nodes) (vector i)))

(define (qlmesh2d-vise->qlat mesh vise)
  (define (node i)
    (vview->vector (qlmesh2d-nodes-ref mesh (vector-ref vise i))))
  (define (x_ i) (vector-ref (node i) 0))
  (define (y_ i) (vector-ref (node i) 1))
  (vector (x_ 0) (x_ 1) (x_ 2) (x_ 3)
          (y_ 0) (y_ 1) (y_ 2) (y_ 3)))


(define (qlmesh2d-unit-square nx ny)
  ()
  (make-qlmesh2d
   (make-vview ns 0 (vector (* nx ny) 2))
   (make-vview qs 0 (vector (* (- nx 1) (- ny 1)) 4))))
