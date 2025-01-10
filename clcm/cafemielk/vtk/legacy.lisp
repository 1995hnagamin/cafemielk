;;;; Legacy VTK

(defpackage :cafemielk/vtk/legacy
  (:use
   :cl
   :cafemielk/util
   :cafemielk/mesh2d)
  (:export
   :legacyvtk-print-cell-types
   :legacyvtk-print-cells
   :legacyvtk-print-header
   :legacyvtk-print-point-scalar
   :legacyvtk-print-points
   :legacyvtk-print-unstructured-grid))

(in-package :cafemielk/vtk/legacy)


(defun legacyvtk-print-header (stream)
  (format stream "# vtk DataFile Version 2.0~%")
  (format stream "sample~%")
  (format stream "ASCII~%"))

(defun legacyvtk-print-points (stream mesh)
  (loop
    :with z-value := 0
    :with nvertex := (mesh2d-trig-vertex-count mesh)
      :initially (format stream "POINTS ~a float~%" nvertex)
    :for vertex-index :below nvertex
    :do
       (with-mesh2d-trig-vertex-accessors (mesh :x x :y y)
         (format stream "~f ~f ~f~%"
                 (x vertex-index) (y vertex-index) z-value))))

(defun legacyvtk-print-cells (stream mesh)
  (loop
    :with trig-npoint := 3
    :with nvise := (mesh2d-trig-vise-count mesh)
      :initially (format stream "CELLS ~a ~a~%" nvise (* 4 nvise))
    :for vise-index :below nvise
    :for vise := (mesh2d-trig-vise-elt mesh vise-index)
    :do
       (format stream "~a ~a ~a ~a~%"
               trig-npoint (aref vise 0) (aref vise 1) (aref vise 2))))

(defun legacyvtk-print-cell-types (stream mesh)
  (loop
    :with nvise := (mesh2d-trig-vise-count mesh)
      :initially (format stream "CELL_TYPES ~a~%" nvise)
    :for i :below nvise
    :do (format stream "5~%")))

(defun legacyvtk-print-unstructured-grid (stream mesh)
  (format stream "DATASET UNSTRUCTURED_GRID~%")
  (legacyvtk-print-points stream mesh)
  (legacyvtk-print-cells stream mesh)
  (legacyvtk-print-cell-types stream mesh))

(defun legacyvtk-print-point-scalar (stream mesh &key vector name)
  (loop
    :with nvertex := (mesh2d-trig-vertex-count mesh)
      :initially (format stream "POINT_DATA ~a~%" nvertex)
                 (format stream "SCALARS ~a float~%" name)
                 (format stream "LOOKUP_TABLE default~%")
    :for vi :across vector
    :do
       (format stream "~f~%" vi)))

;;; Local Variables:
;;; mode: lisp
;;; indent-tabs-mode: nil
;;; End:
