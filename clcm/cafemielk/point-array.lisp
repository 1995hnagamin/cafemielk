;;;; Point Array

(defpackage :cafemielk/point-array
  (:use
   :cl
   :cafemielk/util)
  (:export
   :point-array
   :point-array-2d
   :point-array-count
   :point-array-dimension
   :point-aref-x
   :point-aref-y
   :point-aref-z))
(in-package :cafemielk/point-array)

(deftype point-array (element-type count dimension)
  `(simple-array ,element-type (,count ,dimension)))

(deftype point-array-2d (element-type count)
  `(point-array ,element-type ,count 2))

(defun point-array-count (point-array)
  (declare (type (point-array * * *) point-array))
  (array-dimension point-array 0))

(defun point-array-dimension (point-array)
  (declare (type (point-array * * *) point-array))
  (array-dimension point-array 1))

(declaim (inline point-aref))
(defun point-aref (point-array index dimension-index)
  (aref point-array index dimension-index))

(defmacro define-coordinate-ref (name dimension-index)
  `(progn
     (declaim (inline ,name))
     (defun ,name (point-array index)
       (declare (type (point-array * * *) point-array)
                (type fixnum index))
       (point-aref point-array index ,dimension-index))))

(define-coordinate-ref point-aref-x 0)
(define-coordinate-ref point-aref-y 1)
(define-coordinate-ref point-aref-z 2)

(defmacro define-point-getter (name dimension &key (element-type '*))
  `(progn
     (defun ,name (point-array index)
       (declare (type (point-array ,element-type * ,dimension) point-array)
                (type fixnum index))
       (vector ,@(loop
                   :for i :below dimension
                   :collect `(point-aref point-array index ,i))))))

(define-point-getter coord-vec-2d 2)
(define-point-getter coord-vec-3d 3)

(defmacro with-point-coords (((&rest coords) point-array index) &body body)
  (once-only (point-array index)
    `(let ,(loop :for xi :in coords
                 :for i :from 0
                 :collect `(,xi (point-aref ,point-array ,index ,i)))
       ,@body)))

(defmacro with-point-coords* (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      `(with-point-coords ,(car bindings)
         (with-point-coords* ,(cdr bindings) ,@body))))

;;; Local Variables:
;;; mode: lisp
;;; indent-tabs-mode: nil
;;; End:
