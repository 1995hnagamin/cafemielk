;;;
;;; cafemielk.geom.qlat2d
;;; Quadrilaterals Geometry
;;;

(define-module cafemielk.geom.qlat2d
  (export
   qlat2d-xref
   qlat2d-yref
   )
  )

(select-module cafemielk.geom.qlat2d)

(define-inline (qlat2d-xref qlat i)
  (vector-ref qlat i))

(define-inline (qlat2d-yref qlat i)
  (vector-ref qlat (+ i 4)))
