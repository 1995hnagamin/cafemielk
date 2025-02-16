(defpackage :clcm-delaunay-vtk
  (:use :cl)
  (:local-nicknames (:cm :cafemielk))
  (:export
   :run-analysis))

(in-package :clcm-delaunay-vtk)

(defun create-random-point-array (vertex-count box-size)
  (declare (type fixnum vertex-count)
           (type double-float box-size)
           (values (cm:point-array-2d double-float *) &optional))
  (let ((point-array (make-array `(,vertex-count 2)
                                 :element-type 'double-float)))
    (loop
      :for i :below vertex-count
      :do
         (setf (cm:point-aref-x point-array i) (random box-size))
         (setf (cm:point-aref-y point-array i) (random box-size))
      :finally
         (return point-array))))

(defun convert-vise-vector (vises)
  (loop
    :with trig-count := (length vises)
    :with array := (make-array `(,trig-count 3)
                               :element-type 'fixnum)
    :for i :below trig-count
    :for vise := (aref vises i) :do
      (loop :for j :below 3 :do
        (setf (aref array i j) (aref vise j)))
    :finally (return array)))

(defun run-analysis ()
  (format t "Cafemielk version: ~a~%~%" (cm:cafemielk-version))
  (format t "Bye.~%"))
