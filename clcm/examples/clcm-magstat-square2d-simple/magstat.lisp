(defpackage :clcm-magstat-square2d-simple
  (:use :cl)
  (:local-nicknames (:cm :cafemielk))
  (:export
   :run-analysis))

(in-package :clcm-magstat-square2d-simple)

(defvar *num-mesh-div* 20)

(defvar *mesh* (cm:let1 npoint (1+ *num-mesh-div*)
                 (cm:mesh2d-unit-square npoint npoint)))

(defvar *permeability* 1.0d0) ; H/m

(defvar *current-density* 1.0d0) ; A/m^2

(defmacro array3-tab ((i j k) expr)
  `(make-array
    3
    :element-type 'double-float
    :initial-contents (cm:vec3d-tab (,i ,j ,k) ,expr)))

(defun create-free-equation (mesh)
  "Construct the coefficient matrix and right-hand side vector.
   This function does not consider the boundary conditions."
  (declare (optimize (speed 3)))
  (declare (type double-float *permeability* *current-density*))
  (loop
    :with nvertex := (cm:mesh2d-trig-vertex-count mesh)
    :with rvd := (cm:create-empty-rvd nvertex nvertex
                                      :element-type 'double-float)
    :with rhs := (make-array nvertex
                             :initial-element 0d0
                             :element-type 'double-float)
    :for vise-idx :of-type fixnum :below (cm:mesh2d-trig-vise-count mesh)
    :do
       (loop
         :with vise :of-type (simple-array fixnum (3))
           := (cm:mesh2d-trig-vise-elt mesh vise-idx)
         :with trig :of-type (simple-array double-float (6))
           := (cm:mesh2d-trig-vise->trig2d mesh vise)
         :with trig-area :of-type double-float := (cm:trig2d-area trig)
         :for i :of-type fixnum :from 0
         :for vi :of-type fixnum :across vise
         :do
            (cm:with-trig2d-accessors (trig :x tx :y ty)
              (let ((b (array3-tab (i i+1 i+2) (- (ty i+1) (ty i+2))))
                    (c (array3-tab (i i+1 i+2) (- (tx i+2) (tx i+1)))))
                (declare (type (simple-array double-float (3)) b c))

                ;; Update right-hand side vector.
                (incf (aref rhs vi)
                      (* 1/3 trig-area *current-density*)) ; J_0 S/3

                ;; Update coefficient matrix.
                (loop
                  :for j :of-type fixnum :from 0
                  :for vj :of-type fixnum :across vise
                  :do
                     (cm:rvd-add
                      rvd vi vj
                      (* (+ (* (aref b i) (aref b j))
                            (* (aref c i) (aref c j)))
                         (/ *permeability* (* 4 trig-area))))))))
    :finally (return (values rvd rhs))))

(defun run-analysis ()
  (format t "Hello, World~%")
  (format t "Cafemielk version: ~a~%~%" (cm:cafemielk-version))
  (multiple-value-bind (rvd rhs) (create-free-equation *mesh*)
    (let ((*print-length* 10))
      (format t "RVD:~%~W~%~%RHS:~%~W~%" rvd rhs)))
  (format t "Bye.~%"))
