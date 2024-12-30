;;;; Linear Algebra Library

(defpackage :linalg
  (:use :cl :util)
  (:export
   :dense-matrix
   :make-dense-matrix
   :make-dense-matrix-zero
   :matrix-ref
   :mv-add!
   :vector-addv!
   :vector-rescale!))
(in-package :linalg)

;;;
;;; Vector operations
;;;

(defun vector-rescale! (y c)
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


;;;
;;; Matrix
;;;

(defstruct matrix
  (nrow 0 :type fixnum)
  (ncol 0 :type fixnum))

;;;
;;; Dense Matrix
;;;

(defstruct dense-matrix
  (entries #2a() :type (array number (* *))))

(defmethod matrix-ref ((M dense-matrix) i j)
  (aref (dense-matrix-entries M) i j))

(defun make-dense-matrix-zero (nrow ncol)
  (make-dense-matrix
   :entries (make-array `(,nrow ,ncol)
                        :initial-element 0)))

(defun mv-add! (nrow ncol y M x)
  "y += M * x"
  (loop for i below nrow do
        (loop for j below ncol do
              (incf (aref y i)
                    (* (matrix-ref M i j) (aref x j))))))

;;;
;;; CSR (compressed sparse row)
;;;


(defstruct csr
  (entries)
  (rowptr #() :type (array fixnum (*)))
  (colind #() :type (array fixnum (*))))

(defun csr-addmv! (y nrow ncol A x)
  (declare (ignore ncol))
  (with-slots (entries rowptr colind) A
    (loop :for i :from 0 :below nrow :do
      (loop :for j :from (aref rowptr i) :below (aref rowptr (1+ i)) :do
        (incf (aref y i)
              (* (aref entries j)
                 (aref x (aref colind j))))))))

(defun csr-mv-set! (y nrow ncol A x)
  (loop :for i :from 0 :below nrow :do
    (setf (aref y i) 0))
  (csr-addmv! y nrow ncol A x))

(defun csr-mv (nrow ncol A x)
  (let1 y (make-array nrow :initial-element 0)
    (csr-addmv! y nrow ncol A x)
    y))

(defmethod mv (nrow ncol (A csr) v)
  (csr-mv nrow ncol A v))

(defmethod mv-set! (y nrow ncol (A csr) v)
  (csr-mv-set! y nrow ncol A v))

(defun csr-ref (A i j)
  (with-slots (entries rowptr colind) A
    (loop :for idx :from (aref rowptr i) :below (aref rowptr (1+ i))
            :thereis (and (= j (aref colind idx))
                          (aref entries idx))
          :finally (return 0))))

(defmethod matrix-ref ((M csr) i j)
  (csr-ref M i j))

;;; Local Variables:
;;; mode: lisp
;;; indent-tabs-mode: nil
;;; End:
