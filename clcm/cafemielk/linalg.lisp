;;;; Linear Algebra Library

(provide :linalg)
(defpackage :linalg (:use :cl))
(in-package :linalg)
(export
 '(vector-addv!
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
  (loop for i below (length y)
        do (setf (aref y i) (+ (aref y i) (aref x i)))))

(defun vector-rescale-addv! (y c x)
  "y := c * y + x"
  (loop for i below (length y)
        do (setf (aref y i)
                 (+ (* (aref y i) c) (aref x i)))))

(defun vector-addcv! (y c x)
  "y += c * x"
  (loop for i below (length y)
        do (setf (aref y i) (+ (aref y i) (* (aref x i) c)))))


;(defstruct dense-matrix
;  (data :type (array number (* *))))

;;; Local Variables:
;;; mode: lisp
;;; indent-tabs-mode: nil
;;; End:
