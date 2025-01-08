;;;;
;;;; 2-D Mesh Library
;;;;

(defpackage :cafemielk/mesh2d
  (:use :cl :cafemielk/util)
  (:export
   :create-square-vise-array))
(in-package :cafemielk/mesh2d)


(defun create-square-vise-array (nx ny)
  (loop
    :with array := (make-array `(,(* 2 (1- nx) (1- ny)) 3)
                               :element-type 'fixnum)
    :for j :from 0 :to (- ny 2)
    :do
       (loop
         :for i :from 0 :to (- nx 2)
         :do
            ;;           (j(nx+1)+i)   (j*(nx+1)+i+1)
            ;; j+1                 *---*
            ;;                     |  /|
            ;; [tij = 2j(nx-1)+2i] | / | [2j*(nx-1)+2i+1]
            ;;                     |/  |
            ;; j                   *---*
            ;;        (pij = j*nx+i)   (j*nx+i+1)
            ;;
            ;;                     i  i+1
            (let* ((pij (+ (* j nx) i))
                   (tij (* 2 (- pij j))))
              ;; upper left triangle
              (setf (aref array tij 0) (+ pij nx))
              (setf (aref array tij 1) pij)
              (setf (aref array tij 2) (+ pij nx 1))
              ;; lower right triangle
              (setf (aref array (1+ tij) 0) (+ pij 1))
              (setf (aref array (1+ tij) 1) (+ pij nx 1))
              (setf (aref array (1+ tij) 2) pij)))
    :finally
       (return array)))
