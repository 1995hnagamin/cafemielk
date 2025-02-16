(uiop:define-package :cafemielk/all
  (:nicknames :cafemielk)
  (:use-reexport
   :cafemielk/util
   :cafemielk/geom/trig2d
   :cafemielk/linalg
   :cafemielk/mesh2d
   :cafemielk/mesh/base
   :cafemielk/mesh/delaunay2d
   :cafemielk/mesh2d-quad
   :cafemielk/point-array
   :cafemielk/vtk/legacy
   :cafemielk/package-info))

;;; Local Variables:
;;; mode: lisp
;;; indent-tabs-mode: nil
;;; End:
