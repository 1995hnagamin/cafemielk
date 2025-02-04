;;;; Delaunay triangulation

(defpackage :cafemielk/mesh2d/delaunay
  (:use :cl :cafemielk/geom/trig2d :cafemielk/util)
  (:export
   ))
(in-package :cafemielk/mesh2d/delaunay)

(defun square (nx ny &key (element-type t))
  (loop
    :with array := (make-array `(,(* (1+ nx) (1+ ny)) 2)
                               :element-type element-type)
    :for j :from 0 :to ny
    :do
       (loop
         :for i :from 0 :to nx
         :do
            (setf (aref array (+ (* (1+ nx) j) i) 0)
                  (coerce (/ i nx) element-type))
            (setf (aref array (+ (* (1+ nx) j) i) 1)
                  (coerce (/ j ny) element-type)))
    :finally
       (return array)))

(defun random-range (start end)
  (+ start (random (- end start))))

(defun fisher-yates-shuffle (array start end)
  (loop
    :for i :downfrom (1- end) :above start
    :for j := (random-range start (1+ i))
    :do
       (rotatef (aref array i) (aref array j))
    :finally
       (return array)))

(declaim (inline iota-array))
(defun iota-array (count &key (start 0) (step 1) (element-type t))
  (loop
    :with array := (make-array `(,count)
                               :element-type element-type)
    :for i :from 0 :below count
    :do
       (setf (aref array i) (+ start (* i step)))
    :finally
       (return array)))

(defun copy-vector-adjustable (vector)
  (let ((length (length vector)))
    (make-array `(,length)
                :initial-contents vector
                :adjustable t
                :fill-pointer length)))

(defun delete-nth (n vector)
  (loop
    :for i :from (1+ n) :below (length vector)
    :do
       (rotatef (aref vector (1- i)) (aref vector i))
    :finally
       (vector-pop vector)))

(defun vise->trig (points vise)
  `#(,(aref (aref points (aref vise 0)) 0)
     ,(aref (aref points (aref vise 0)) 1)
     ,(aref (aref points (aref vise 1)) 0)
     ,(aref (aref points (aref vise 1)) 1)
     ,(aref (aref points (aref vise 2)) 0)
     ,(aref (aref points (aref vise 2)) 1)))

(declaim (inline point-array-xref))
(defun point-array-xref (point-array i)
  (aref point-array i 0))

(declaim (inline point-array-yref))
(defun point-array-yref (point-array i)
  (aref point-array i 1))

(declaim (inline point-array-count))
(defun point-array-count (point-array)
  (array-dimension point-array 0))

(declaim (inline point-array-nth))
(defun point-array-nth (n point-array)
  (declare (type fixnum n)
           (type (array * (* *)) point-array))
  `#(,(point-array-xref point-array n)
     ,(point-array-yref point-array n)))

(defmacro lexicographic< ((i j) (param) &rest clauses)
  (once-only (i j)
    (reduce
     #'(lambda (clause tail)
         (destructuring-bind (form &key (less '<) (equ '=)) clause
           (with-gensyms (form-* form-i form-j)
             `(flet ((,form-* (,param) ,form))
                (declare (inline ,form-*))
                (let ((,form-i (,form-* ,i))
                      (,form-j (,form-* ,j)))
                  (or (,less ,form-i ,form-j)
                      (and (,equ ,form-i ,form-j)
                           ,tail)))))))
     clauses
     :from-end t
     :start 0 :end (length clauses)
     :initial-value nil)))

(defun get-shuffled-indexes (point-array)
  (declare (type (array * (* *)) point-array))
  (flet ((lex< (i j)
           (declare (type fixnum i j))
           (lexicographic< (i j)
               (k)
               ((point-array-yref point-array k))
               ((point-array-xref point-array k)))))
    (let* ((npoint (point-array-count point-array))
           (indexes (iota-array npoint :element-type 'fixnum))
           (highest-point-index (loop
                                  :with m := 0
                                  :for i :from 1 :below npoint
                                  :when (lex< m i)
                                    :do (setf m i)
                                  :finally
                                     (return m))))
      (declare (type (array fixnum (*)) indexes)
               (type fixnum npoint))
      (rotatef (aref indexes 0) (aref indexes highest-point-index))
      (fisher-yates-shuffle indexes 1 npoint))))

;;; Local Variables:
;;; mode: lisp
;;; indent-tabs-mode: nil
;;; End:
