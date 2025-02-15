;;;; Mesh2D Basics

(defpackage :cafemielk/mesh2d/base
  (:use
   :cl
   :cafemielk/point-array
   :cafemielk/util)
  (:export
   :vertex-index-sequence))
(in-package :cafemielk/mesh2d/base)

(deftype vertex-index-sequence (vertex-count)
  `(simple-array fixnum (,vertex-count)))
