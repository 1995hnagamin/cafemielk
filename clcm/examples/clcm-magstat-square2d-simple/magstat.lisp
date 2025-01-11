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
    :for vise-index :of-type fixnum :below (cm:mesh2d-trig-vise-count mesh)
    :for vise :of-type (simple-array fixnum (3))
      := (cm:mesh2d-trig-vise-elt mesh vise-index)
    :for trig :of-type (simple-array double-float (6))
      := (cm:mesh2d-trig-vise->trig2d mesh vise)
    :for trig-area :of-type double-float := (cm:trig2d-area trig)
    :do
       ;; Update right-hand side vector.
       (loop :for vi :of-type fixnum :across vise :do
         (incf (aref rhs vi) (* 1/3 trig-area *current-density*))) ; J_0 S/3

       ;; Update coefficient matrix.
       (cm:with-trig2d-accessors (trig :x tx :y ty)
         (loop
           :with b :of-type (simple-array double-float (3))
             := (array3-tab (i i+1 i+2) (- (ty i+1) (ty i+2)))
           :with c :of-type (simple-array double-float (3))
             := (array3-tab (i i+1 i+2) (- (tx i+2) (tx i+1)))
           :for i :of-type fixnum :from 0
           :for vi :of-type fixnum :across vise
           :do
              (loop
                :for j :of-type fixnum :from 0
                :for vj :of-type fixnum :across vise
                :do
                   (cm:rvd-add
                    rvd vi vj
                    (* (+ (* (aref b i) (aref b j))
                          (* (aref c i) (aref c j)))
                       (/ *permeability* (* 4 trig-area)))))))
    :finally (return (values rvd rhs))))

(defun create-arith-seq (&key size initial-value step (element-type t))
  (loop
    :with array := (make-array size :element-type element-type)
    :for i :from 0 :below size
    :for value :from initial-value :by step
    :do (setf (aref array i) value)
    :finally (return array)))

(defun unit-square-boundary-vertices (ndiv)
  ;;
  ;;       R       U
  ;; n^2+n *-------* n^2+2n
  ;;   ... |       | ...
  ;;  2n+2 |       | 3n+1
  ;;   n+1 |       | 2n+1
  ;;     0 *-------* n
  ;;       B       L
  ;;
  (let ((bottom (create-arith-seq :size ndiv
                                  :initial-value 0
                                  :step 1
                                  :element-type 'fixnum))
        (top (create-arith-seq :size ndiv
                               :initial-value (+ (expt ndiv 2) ndiv 1)
                               :step 1
                               :element-type 'fixnum))
        (left (create-arith-seq :size ndiv
                                :initial-value ndiv
                                :step (1+ ndiv)
                                :element-type 'fixnum))
        (right (create-arith-seq :size ndiv
                                 :initial-value (1+ ndiv)
                                 :step (1+ ndiv)
                                 :element-type 'fixnum)))
    (sort (concatenate `(simple-array fixnum (,(* 4 ndiv)))
                       bottom top left right)
          #'<)))

(defvar *dirichlet-vertex-indices*
  (unit-square-boundary-vertices *num-mesh-div*))

(defun create-equation (mesh)
  "Construct the coefficient matrix and right-hand side vector
   considering the boundary conditions."
  (multiple-value-bind (rvd rhs) (create-free-equation mesh)
    (loop
      :with dirichlet-value := 0.0d0
      :for vk~ :across *dirichlet-vertex-indices*
      :with nvertex := (cm:mesh2d-trig-vertex-count mesh)
      :do
         ;; Remove off-diagonal elements from vk~th row
         (loop
           :for vj :below nvertex
           :when (/= vk~ vj)
             :do (cm:rvd-delete! rvd vk~ vj))
         (setf (cm:get-rvd rvd vk~ vk~) 1.0d0)
         ;; Update RHS vector
         (setf (aref rhs vk~) dirichlet-value)
         ;; Remove off-diagonal elements from vk~th column
         (loop
           :for vi :below nvertex
           :when (/= vk~ vi)
             :do
                (decf (aref rhs vi) (* (cm:get-rvd rvd vi vk~)
                                       dirichlet-value))
                (cm:rvd-delete! rvd vi vk~))
      :finally
         (return (values (cm:coo->csr
                          (cm:rvd->coo rvd :element-type 'double-float)
                          :element-type 'double-float)
                         rhs)))))

(defun run-analysis ()
  (format t "Hello, World~%")
  (format t "Cafemielk version: ~a~%~%" (cm:cafemielk-version))
  (multiple-value-bind (A b) (create-equation *mesh*)
    (let ((sol (cm:cg-solve A b :eps 1e-8 :max-iter 1000))
          (*print-length* 450))
      (with-open-file (stream #P"output.vtk"
                              :if-does-not-exist :create
                              :if-exists :supersede
                              :direction :output)
        (cm:legacyvtk-print-header stream)
        (cm:legacyvtk-print-unstructured-grid stream *mesh*)
        (cm:legacyvtk-print-point-scalar stream *mesh*
                                         :vector sol
                                         :name "A"))
      (format t "Solution:~%~W~%" sol)))
  (format t "Bye.~%"))
