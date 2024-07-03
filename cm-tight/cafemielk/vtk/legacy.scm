;;;
;;; cafemielk.vtk.legacy
;;;

(define-module cafemielk.vtk.legacy
  (use cafemielk.mesh)
  (export
   legacyvtk-print-cell-types
   legacyvtk-print-cells
   legacyvtk-print-header
   legacyvtk-print-point-scalar
   legacyvtk-print-points
   legacyvtk-print-unstruct-grid
   )
  )

(select-module cafemielk.vtk.legacy)

(define (legacyvtk-print-header port)
  (display "# vtk DataFile Version 2.0\n" port)
  (display "sample\n" port)
  (display "ASCII\n" port))

(define (legacyvtk-print-points mesh port)
  (format port "POINTS ~s float\n"
          (mesh2d-nodes-length mesh))
  (mesh2d-nodes-for-each-with-index
   (lambda (k x y)
     (format port "~s ~s 0\n"  ;; x y z
             x y))
   mesh))

(define (legacyvtk-print-cells mesh port)
  (define N (mesh2d-triangles-length mesh))
  (format port "CELLS ~s ~s\n"
          N (* 4 N))
  (mesh2d-vise-for-each
   (lambda (vise)
     (format port "3 ~s ~s ~s\n"
             (vector-ref vise 0)
             (vector-ref vise 1)
             (vector-ref vise 2)))
   mesh))

(define (legacyvtk-print-cell-types mesh port)
  (format port "CELL_TYPES ~s\n"
          (mesh2d-triangles-length mesh))
  (mesh2d-vise-for-each
   (lambda (vise)
     (display "5\n" port))
   mesh))

(define (legacyvtk-print-unstruct-grid mesh port)
  (display "DATASET UNSTRUCTURED_GRID\n" port)
  (legacyvtk-print-points mesh port)
  (legacyvtk-print-cells mesh port)
  (legacyvtk-print-cell-types mesh port))

(define (legacyvtk-print-point-scalar mesh vec name port)
  (format port "POINT_DATA ~s\n" (mesh2d-nodes-length mesh))
  (format port "SCALARS ~A float\n" name)
  (display "LOOKUP_TABLE default\n" port)
  (vector-for-each
   (lambda (vi)
     (format port "~s\n" vi))
   vec))
