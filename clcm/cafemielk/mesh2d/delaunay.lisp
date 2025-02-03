;;;; Delaunay triangulation

(defpackage :cafemielk/mesh2d/delaunay
  (:use :cl :cafemielk/geom/trig2d)
  (:export
   ))
(in-package :cafemielk/mesh2d/delaunay)

(defun random-begin-end (start end)
  (+ start (random (- end start))))

(defun fisher-yates-shuffle (array start end)
  (loop
    :for i :downfrom (1- end) :above start
    :for j := (random-begin-end start (1+ i))
    :do
       (rotatef (aref array i) (aref array j))
    :finally
       (return array)))
