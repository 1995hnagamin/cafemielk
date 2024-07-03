;;;
;;; cafemielk.mesh.qlat
;;; Quadrilateral mesh
;;;

(define-module cafemielk.mesh.qlat
  (export
   <qlat2d>
   qlat2d-xref
   qlat2d-yref
   qlat2d-nodes-ref
   qlat2d-qnix->qlat
   qlat2d-qnix-ref
   )
  )

(select-module cafemielk.mesh.qlat)

(define-inline (qlat2d-xref qlat i)
  (vector-ref qlat i))

(define-inline (qlat2d-yref qlat i)
  (vector-ref qlat (+ i 4)))

(define-class <qlat2d> ()
  ((nodes :init-keyword :nodes)
   (qnixs :init-keyword :qnixs)))

(define (make-qlat2d nodes qnixs)
  (make <qlat2d> :nodes nodes :vises vises))

(define (qlat2d-qlats-length mesh)
  (vview-length (slot-ref 'vises)))

(define (qlat2d-nodes-ref mesh i)
  (vview-cut (slot-ref mesh 'nodes) (vector i)))

(define (qlat2d-vise->qlat mesh vise)
  (define (node i)
    (vview->vector (qlat2d-nodes-ref mesh (vector-ref vise i))))
  (define (x_ i) (vector-ref (node i) 0))
  (define (y_ i) (vector-ref (node i) 1))
  (vector (x_ 0) (x_ 1) (x_ 2) (x_ 3)
          (y_ 0) (y_ 1) (y_ 2) (y_ 3)))


(define (qlat2d-unit-square nx ny)
  ()
  (mak2-qlat2d
   (make-vview ns 0 (vector (* nx ny) 2))
   (make-vview qs 0 (vector (* (- nx 1) (- ny 1)) 4))))
