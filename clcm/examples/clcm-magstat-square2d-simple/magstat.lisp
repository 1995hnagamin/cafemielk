(defpackage :clcm-magstat-square2d-simple
  (:use :cl)
  (:local-nicknames (:cm :cafemielk))
  (:export
   :run-analysis))

(in-package :clcm-magstat-square2d-simple)

(defvar *num-mesh-div* 20)

(defvar *mesh* (cm:let1 npoint (1+ *num-mesh-div*)
                 (cm:mesh2d-unit-square npoint npoint)))

(defvar *permeability* 1) ; H/m

(defvar *current-density* 1) ; A/m^2

(defun create-free-equation (mesh)
  "Construct the coefficient matrix and right-hand side vector.
   This function does not consider the boundary conditions."
  (loop
    :with nvertex := (cm:mesh2d-trig-vertex-count mesh)
    :with rvd := (cm:create-empty-rvd nvertex nvertex
                                      :element-type 'double-float)
    :with rhs := (make-array nvertex
                             :initial-element 0d0
                             :element-type 'double-float)
    :for vise-idx :below (cm:mesh2d-trig-vise-count mesh)
    :do
       (loop
         :with vise := (cm:mesh2d-trig-vise-elt mesh vise-idx)
         :with trig := (cm:mesh2d-trig-vise->trig2d mesh vise)
         :with trig-area := (cm:trig2d-area trig)
         :with b := (cm:vec3d-tab (i i+1 i+2)
                                  (- (cm:trig2d-yref trig i+1)
                                     (cm:trig2d-yref trig i+2)))
         :with c := (cm:vec3d-tab (i i+1 i+2)
                                  (- (cm:trig2d-xref trig i+2)
                                     (cm:trig2d-xref trig i+1)))
         :for i :from 0
         :for vi :of-type fixnum :across vise
         :do
            ;; Update right-hand side vector.
            (incf (aref rhs vi)
                  (* 1/3 trig-area *current-density*)) ; J_0 S/3
            ;; Update coefficient matrix.
            (loop
              :for j :from 0
              :for vj :of-type fixnum :across vise
              :do
                 (incf (cm:get-rvd rvd vi vj)
                       (* (+ (* (aref b i) (aref b j))
                             (* (aref c i) (aref c j)))
                          (/ *permeability* (* 4 trig-area))))))
    :finally (return (values rvd rhs))))

(defun run-analysis ()
  (format t "Hello, World~%")
  (format t "Cafemielk version: ~a~%" (cm:cafemielk-version)))
