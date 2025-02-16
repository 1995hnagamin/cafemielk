(defpackage :clcm-delaunay-vtk
  (:use :cl)
  (:local-nicknames (:cm :cafemielk))
  (:export
   :run-triangulation))

(in-package :clcm-delaunay-vtk)

(defvar *vertex-count* 2000)
(defvar *box-size* 10.0d0)

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

(defun run-triangulation ()
  (format t "Cafemielk version: ~a~%~%" (cm:cafemielk-version))
  (let* ((point-array (create-random-point-array
                       *vertex-count*
                       *box-size*))
         (vises (cm:delaunay-triangulate point-array))
         (mesh (cm:make-mesh2d-trig
                :vertices point-array
                :vises (convert-vise-vector vises))))
    (with-open-file (stream #P"output.vtk"
                            :if-does-not-exist :create
                            :if-exists :supersede
                            :direction :output)
      (cm:legacyvtk-print-header stream)
      (cm:legacyvtk-print-unstructured-grid stream mesh)))
  (format t "Bye.~%"))
