;;;; Cafemielk Ulitity Library

(provide :util)
(defpackage :util (:use :cl))
(in-package :util)
(export
 '(vec3d-tab))

(defmacro let1 (var init &body body)
  `(let ((,var ,init))
     ,@body))

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

(defun cross2d (u v)
  (- (* (aref u 0) (aref v 1))
     (* (aref u 1) (aref v 0))))

(defun cross3d (u v)
  (vec3d-tab
      (i i+1 i+2)
      (- (* (aref u i+1) (aref v i+2))
         (* (aref u i+2) (aref v i+1)))))

(defun dot-product (u v)
  (loop :for i :from 0 :below (array-dimension u 0)
        :sum (* (aref u i) (aref v i))))

