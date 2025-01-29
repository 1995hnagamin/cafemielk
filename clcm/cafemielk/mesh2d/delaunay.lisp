;;;; Delaunay triangulation

(defpackage :cafemielk/mesh2d/delaunay
  (:use :cl :cafemielk/geom/trig2d)
  (:export
   ))
(in-package :cafemielk/mesh2d/delaunay)

(defun random-begin-end (begin end)
  (+ begin (random (- end begin))))

(defun fisher-yates-shuffle (array begin end)
  (loop
    :for i :downfrom (1- end) :above begin
    :for j := (random-begin-end begin (1+ i))
    :do
       (rotatef (aref array i) (aref array j))
    :finally
       (return array)))
