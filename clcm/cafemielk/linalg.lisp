;;;; Linear Algebra Library

(provide :linalg)
(defpackage :linalg (:use :cl))
(in-package :linalg)
(export
 '(dense-matrix
   make-dense-matrix
   make-dense-matrix-zero
   matrix-ref
   mv-add!
   vector-addv!
   vector-scale!))

;;;
;;; Vector operations
;;;

(defun vector-scale! (y c)
  "y *= c"
  (loop for i below (length y)
        do (setf (aref y i) (* (aref y i) c))))

(defun vector-addv! (y x)
  "y += x"
  (loop for i below (length y) do
        (incf (aref y i) (aref x i))))

(defun vector-rescale-addv! (y c x)
  "y := c * y + x"
  (loop for i below (length y) do
        (setf (aref y i)
              (+ (* (aref y i) c) (aref x i)))))

(defun vector-addcv! (y c x)
  "y += c * x"
  (loop for i below (length y) do
        (incf (aref y i) (* (aref x i) c))))

(defstruct dense-matrix
  (entries #2a() :type (array number (* *))))

(defmethod matrix-ref ((M dense-matrix) i j)
  (aref (dense-matrix-entries M) i j))

(defun make-dense-matrix-zero (nrow ncol)
  (make-dense-matrix
   :data (make-array `(,nrow ,ncol)
                     :initial-element 0)))

(defun mv-add! (nrow ncol y M x)
  "y += M * x"
  (loop for i below nrow do
        (loop for j below ncol do
              (incf (aref y i)
                    (* (matrix-ref M i j) (aref x j))))))



;;; Local Variables:
;;; mode: lisp
;;; indent-tabs-mode: nil
;;; End:
