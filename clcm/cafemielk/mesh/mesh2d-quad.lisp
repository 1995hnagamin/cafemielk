(defpackage :cafemielk/mesh/mesh2d-quad
  (:use
   :cl
   :cafemielk/util)
  (:import-from :cafemielk/mesh/mesh2d-trig
                :create-square-point-array)
  (:export
   :mesh2d-quad
   :mesh2d-quad-unit-square))
(in-package :cafemielk/mesh/mesh2d-quad)

(defstruct mesh2d-quad
  (vertices nil :type (array * (* 2)))
  (vises nil :type (array fixnum (* 4))))

(defun create-square-quad-vise-array (nx ny)
  (loop
    :with vises :of-type (simple-array fixnum (* 4))
      := (make-array `(,(* (1- nx) (1- ny)) 4) :element-type 'fixnum)
    :for j :of-type fixnum :from 0 :below (1- ny)
    :do
       (loop
         :for i :of-type fixnum :from 0 :below (1- nx)
         :for pij :of-type fixnum := (+ (* j nx) i)
         :for position :of-type fixnum := (+ (* (1- nx) j) i)
         :do
            (setf (aref vises position 0) pij)
            (setf (aref vises position 1) (+ pij 1))
            (setf (aref vises position 2) (+ pij 1 nx))
            (setf (aref vises position 3) (+ pij nx)))
    :finally
       (return vises)))

(defun mesh2d-quad-unit-square (nx ny)
  (make-mesh2d-quad
   :vertices (create-square-point-array
              (vector-linspace 0d0 1d0 nx :element-type 'double-float)
              (vector-linspace 0d0 1d0 ny :element-type 'double-float)
              :element-type 'double-float)
   :vises (create-square-quad-vise-array nx ny)))
