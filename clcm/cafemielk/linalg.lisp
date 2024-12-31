;;;; Linear Algebra Library

(defpackage :linalg
  (:use :cl :util)
  (:export
   :dense-matrix
   :make-dense-matrix
   :make-dense-matrix-zero
   :matrix-ref
   :mv
   :mv-add!
   :mv-set!
   :vector-addv!
   :vector-rescale!))
(in-package :linalg)

;;;
;;; Vector operations
;;;

(defun vector-rescale! (y c)
  "y *= c"
  (loop :for i :below (length y) :do
    (setf (aref y i) (* (aref y i) c))))

(defun vector-addv! (y x)
  "y += x"
  (loop :for i :below (length y) :do
    (incf (aref y i) (aref x i))))

(defun vector-rescale-addv! (y c x)
  "y := c * y + x"
  (loop :for i :below (length y) :do
    (setf (aref y i)
          (+ (* (aref y i) c) (aref x i)))))

(defun vector-addcv! (y c x)
  "y += c * x"
  (loop :for i :below (length y) :do
    (incf (aref y i) (* (aref x i) c))))


;;;
;;; Matrix
;;;

(defstruct matrix
  (nrow 0 :type fixnum)
  (ncol 0 :type fixnum))

(defgeneric mv (matrix vector)
  (:documentation
   "Returns the matrix-vector product of MATRIX and VECTOR."))

(defgeneric mv-add! (output-vector matrix vector)
  (:documentation
   "Compute the matrix-vector product of MATRIX and VECTOR."))

(defgeneric mv-set! (output-vector matrix vector)
  (:documentation
   "Compute the matrix-vector product of MATRIX and VECTOR.
The result is contained in OUTPUT-VECTOR."))

(defgeneric matrix-ref (matrix i j)
  (:documentation
   "Returns the (I, J) element of MATRIX."))

;;;
;;; Dense Matrix
;;;

(defstruct (dense-matrix (:include matrix))
  (entries nil :type (simple-array * (* *))))

(defmethod matrix-ref ((M dense-matrix) i j)
  (aref (dense-matrix-entries M) i j))

(defun make-dense-matrix-zero (nrow ncol)
  (make-dense-matrix
   :entries (make-array `(,nrow ,ncol)
                        :initial-element 0)))

(defun dense-matrix-mv-add! (y A x)
  (with-slots (nrow ncol entries) A
    (loop :for i :below nrow :do
      (loop :for j :below ncol :do
        (incf (aref y i)
              (* (matrix-ref A i j)
                 (aref x j)))))))

(defmethod mv-add! (y (A dense-matrix) x)
  (dense-matrix-mv-add! y A x))

;;;
;;; CSR (compressed sparse row)
;;;

(defstruct (csr (:include matrix))
  (entries nil :type (simple-array * (*)))
  (rowptr nil :type (simple-array fixnum (*)))
  (colind nil :type (simple-array fixnum (*))))

(defun csr-addmv! (y nrow entries rowptr colind x)
  (loop :for i :from 0 :below nrow :do
    (loop :for j :from (aref rowptr i) :below (aref rowptr (1+ i)) :do
      (incf (aref y i)
            (* (aref entries j)
               (aref x (aref colind j)))))))

(defmacro with-expand (symbol expr-list body-expr)
  (with-gensyms (macro-name)
    `(symbol-macrolet ((,symbol ',expr-list))
       (macrolet ((,macro-name () ,body-expr))
         (,macro-name)))))

(defmethod mv-add! (y (A csr) x)
  (with-expand slots (nrow entries rowptr colind)
    `(with-slots ,slots A
       (csr-addmv! y ,@slots x))))

(defun csr-mv-set! (y nrow entries rowptr colind x)
  (loop :for i :from 0 :below nrow :do
    (setf (aref y i) 0))
  (csr-addmv! y nrow entries rowptr colind x))

(defun csr-mv (nrow entries rowptr colind x)
  (let1 y (make-array nrow :initial-element 0)
    (csr-addmv! y nrow entries rowptr colind x)
    y))

(defmethod mv ((A csr) v)
  (with-slots (nrow entries rowptr colind) A
    (csr-mv nrow entries rowptr colind v)))

(defmethod mv-set! (y (A csr) v)
  (with-slots (nrow entries rowptr colind) A
    (csr-mv-set! y nrow entries rowptr colind v)))

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
