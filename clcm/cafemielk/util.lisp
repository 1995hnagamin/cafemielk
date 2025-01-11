;;;; Cafemielk Utility Library

(defpackage :cafemielk/util
  (:use :cl)
  (:export
   :let1
   :if-let1
   :when-let1
   :let/goer
   :list1-if
   :once-only
   :with-gensyms
   :aref-let
   :aref-let1
   :aref-macrolet
   :aref-macrolet1
   :clone-array-with-zeros
   :fill-array-with
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

(defmacro if-let1 (var condition then else)
  `(let1 ,var ,condition
         (if ,var ,then ,else)))

(defmacro when-let1 (var condition &body body)
  `(let1 ,var ,condition
     (when ,var ,@body)))

(defmacro let/goer (binds jumper-name &body body)
  (let ((jumper-params (loop :for bind :in binds :collect (gensym)))
        (jumper-tag (gensym "JUMP"))
        (block-tag (gensym)))
    `(block ,block-tag
       (let ,binds
         (tagbody
            ,jumper-tag
            (return-from ,block-tag
              (flet ((,jumper-name ,jumper-params
                       (psetq ,@(loop :for bind :in binds
                                      :for param :in jumper-params
                                      :append (list (car bind) param)))
                       (go ,jumper-tag)))
                (declare (inline ,jumper-name))
                ,@body)))))))

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

(defmacro aref-let1 ((&rest vars) array &body body)
  (once-only (array)
    `(let ,(loop :for var :in vars
                 :for i :from 0
                 :collect `(,var (aref ,array ,i)))
       ,@body)))

(defmacro aref-let ((&rest binds-list) &body body)
  (let1 array-names (loop :for binds :in binds-list :collect (gensym))
    `(let ,(loop :for (vars array) :in binds-list
                 :for a :in array-names
                 :collect `(,a ,array))
       (let ,(loop :for (vars array) :in binds-list
                   :for a :in array-names
                   :append (loop :for var :in vars
                                 :for i :from 0
                                 :collect `(,var (aref ,a ,i))))
         ,@body))))

(defmacro aref-macrolet1 ((&rest vars) array &body body)
  (once-only (array)
    `(macrolet ,(loop :for var :in vars
                      :for i :from 0
                      :collect `(,var () `(aref ,',array ,',i)))
       ,@body)))

(defmacro aref-macrolet ((&rest binds-list) &body body)
  (let1 array-names (loop :for binds :in binds-list :collect (gensym))
    `(let ,(loop :for (vars array) :in binds-list
                 :for a :in array-names
                 :collect `(,a ,array))
       (macrolet ,(loop :for (vars array) :in binds-list
                        :for a :in array-names
                        :append (loop :for var :in vars
                                      :for i :from 0
                                      :collect `(,var () `(aref ,',a ,',i))))
         ,@body))))

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

(defun vector-linspace (min max size &key (element-type t))
  (let ((vec (make-array size
                         :element-type element-type))
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
  (loop :for i :from 0 :below (length u)
        :sum (* (aref u i) (aref v i))))

(declaim (inline fill-array-with-zero))
(defun fill-array-with (array value)
  (loop
    :with fill := (coerce value (array-element-type array))
    :for i :below (length array)
    :do (setf (aref array i) fill)))

(declaim (inline clone-array-with-zeros))
(defun clone-array-with-zeros (prototype-array)
  (let ((element-type (array-element-type prototype-array)))
    (make-array (length prototype-array)
              :initial-element (coerce 0 element-type)
              :element-type element-type)))

;;; Local Variables:
;;; mode: lisp
;;; indent-tabs-mode: nil
;;; End:
