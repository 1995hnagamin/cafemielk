;;;; Linear Algebra Library

(defpackage :cafemielk/linalg
  (:use :cl :cafemielk/util)
  (:export
   :coo
   :coo-p
   :make-coo
   :csr
   :csr-p
   :make-csr
   :coo->csr
   :dense-matrix
   :dense-matrix-p
   :make-dense-matrix
   :make-dense-matrix-zero
   :matrix-ref
   :mv
   :mv-add!
   :mv-set!
   :rvd
   :rvd-p
   :make-rvd
   :create-empty-rvd
   :rvd->csr
   :vector-addv!
   :vector-rescale!))
(in-package :cafemielk/linalg)

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

(defgeneric mv (matrix vector &key element-type)
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
  "y += A * x"
  (with-slots (nrow ncol entries) A
    (loop :for i :below nrow :do
      (loop :for j :below ncol :do
        (incf (aref y i)
              (* (matrix-ref A i j)
                 (aref x j)))))))

(defmethod mv-add! (y (A dense-matrix) x)
  (dense-matrix-mv-add! y A x))


;;;
;;; COO (coordinate format)
;;;

(defstruct (coo (:include matrix))
  (entries nil :type (array * (*)))
  (rowind nil :type (array * (*)))
  (colind nil :type (array * (*))))

;;;
;;; RVD (row-based vector of dictionaries)
;;;

(defstruct (rvd (:include matrix))
  (rows nil :type (array * (*))))

(defun create-empty-rvd (nrow ncol)
  (make-rvd
   :nrow nrow
   :ncol ncol
   :rows (loop :with rows := (make-array nrow)
               :for i :below nrow
               :do (setf (aref rows i) (make-hash-table))
               :finally (return rows))))

(defmacro rvdf (A i j &optional (default nil))
  `(gethash ,j (aref (rvd-rows ,A) ,i)
            ,@(list1-if default default)))

(defun rvd-ref (A i j)
  (nth-value 0 (rvdf A i j 0)))

(defmethod matrix-ref ((A rvd) i j)
  (rvd-ref A i j))

(defun rvd-set! (A i j value)
  (setf (rvdf A i j) value))

(defun rvd-inc! (A i j increment)
  (multiple-value-bind (old-value key-exist-p) (rvdf A i j)
    (setf (rvdf A i j)
          (if key-exist-p
              (+ old-value increment)
              increment))))

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

(defun csr-mv (nrow entries rowptr colind x
               &key (element-type (array-element-type x)))
  (let1 y (make-array nrow :initial-element 0 :element-type element-type)
    (csr-addmv! y nrow entries rowptr colind x)
    y))

(defmethod mv ((A csr) v &key (element-type (array-element-type v)))
  (with-slots (nrow entries rowptr colind) A
    (csr-mv nrow entries rowptr colind v :element-type element-type)))

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

(defun rowind->rowptr (rowind nrow)
  (declare (type fixnum nrow))
  (let ((rowptr (make-array (1+ nrow)
                            :element-type 'fixnum))
        (rowind-size (array-dimension rowind 0)))
    (setf (aref rowptr 0) 0)
    (let/goer ((irow 0) (idx 0)) go-loop
      ;; irow: index of the last updated element in rowptr
      ;; idx:  index of the first unseeked element in rowind
      (declare (type fixnum irow idx))
      (cond
        ((= idx rowind-size)
         (loop :for ir :from (1+ irow) :to nrow :do
           (setf (aref rowptr ir) rowind-size))
         rowptr)
        ((> (aref rowind idx) irow)
         (setf (aref rowptr (1+ irow)) idx)
         (go-loop (1+ irow) idx))
        (t
         (go-loop irow (1+ idx)))))))

(defun coo->csr (A &key (element-type t))
  (declare (type coo A))
  (with-slots (nrow ncol entries rowind colind) A
    (make-csr
     :nrow nrow
     :ncol ncol
     :entries (make-array (array-dimension entries 0)
                          :initial-contents entries
                          :element-type element-type)
     :colind (make-array (array-dimension colind 0)
                         :initial-contents colind
                         :element-type 'fixnum)
     :rowptr (rowind->rowptr rowind nrow))))

;;; Local Variables:
;;; mode: lisp
;;; indent-tabs-mode: nil
;;; End:
