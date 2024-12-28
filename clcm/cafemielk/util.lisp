;;;; Cafemielk Ulitity Library

(provide :util)
(defpackage :util (:use :cl))
(in-package :util)
(export
 '(vec3d-tab))


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

(defmacro vec3d-tab ((i j k) expr)
  `(collect-substitute-a3 vector (,i ,j ,k) ,expr))
