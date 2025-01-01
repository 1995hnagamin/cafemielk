;;;; Cafemielk Utility Library

(defpackage :cafemielk/util
  (:use :cl)
  (:export
   :let1
   :list1-if
   :once-only
   :with-gensyms
   :linspace
   :non-negative-p
   :non-positive-p
   :vector-linspace
   :collect-substitute
   :collect-substitute-a3
   :every-substitute-a3
   :vec3d-tab
   :cross2d
   :cross3d
   :dot-product))
(in-package :cafemielk/util)


(defmacro let1 (var init &body body)
  `(let ((,var ,init))
     ,@body))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop :for n :in names :collect `(,n (gensym)))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop :for n :in names :collect (gensym))))
    `(let (,@(loop :for g :in gensyms
                   :collect `(,g (gensym))))
       `(let (,,@(loop :for g :in gensyms
                       :for n :in names
                       :collect ``(,,g ,,n)))
          ,(let (,@(loop :for n :in names
                         :for g :in gensyms
                         :collect `(,n ,g)))
             ,@body)))))


(defmacro list1-if (condition then)
  `(if ,condition
       (list ,then)
       nil))

(declaim (inline non-negative-p))
(defun non-negative-p (x)
  (not (minusp x)))

(declaim (inline non-positive-p))
(defun non-positive-p (x)
  (not (plusp x)))

(defun linspace (min max size)
  (let1 step (/ (- max min) (1- size))
    (loop :for i :from 0 :below size
          :collecting (+ min (* step i)))))

(defun vector-linspace (min max size)
  (let ((vec (make-array size))
        (step (/ (- max min) (1- size))))
    (loop :for i :from 0 :below size
          :do (setf (aref vec i) (+ min (* step i))))
    vec))

(defmacro collect-substitute (collect (params &rest args-list) expr)
  `(,collect
       ,@(loop :for args :in args-list
               :collecting
               `(let ,(loop :for param :in params
                            :for arg :in args
                            :collecting `(,param ,arg))
                  (declare (ignorable ,@params))
                  ,expr))))

(defmacro collect-substitute-a3 (collect (i j k) expr)
  `(collect-substitute
       ,collect
       ((,i ,j ,k) (0 1 2) (1 2 0) (2 0 1))
       ,expr))

(defmacro every-substitute-a3 ((i j k) expr)
  `(collect-substitute-a3 and (,i ,j ,k) ,expr))

(defmacro vec3d-tab ((i j k) expr)
  `(collect-substitute-a3 vector (,i ,j ,k) ,expr))

(declaim (inline cross2d))
(defun cross2d (u v)
  (- (* (aref u 0) (aref v 1))
     (* (aref u 1) (aref v 0))))

(declaim (inline cross3d))
(defun cross3d (u v)
  (vec3d-tab
      (i i+1 i+2)
      (- (* (aref u i+1) (aref v i+2))
         (* (aref u i+2) (aref v i+1)))))

(declaim (inline dot-product))
(defun dot-product (u v)
  (loop :for i :from 0 :below (array-dimension u 0)
        :sum (* (aref u i) (aref v i))))
