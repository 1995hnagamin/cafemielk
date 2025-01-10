;;;;
;;;; 2-D Mesh Library
;;;;

(defpackage :cafemielk/mesh2d
  (:use
   :cl
   :cafemielk/geom/trig2d
   :cafemielk/util)
  (:export
   :mesh2d-trig
   :mesh2d-trig-p
   :make-mesh2d-trig
   :mesh2d-trig-vertex-count
   :mesh2d-trig-vertex-elt
   :mesh2d-trig-vise-count
   :mesh2d-trig-vise-elt
   :mesh2d-trig-vise->trig2d
   :mesh2d-unit-square
   :create-square-point-array
   :create-square-vise-array))
(in-package :cafemielk/mesh2d)


(defstruct mesh2d-trig
  (vertices nil :type (array * (* 2)))
  (vises nil :type (array fixnum (* 3))))

(defun mesh2d-trig-coordinate-type (mesh)
  (array-element-type (mesh2d-trig-vertices mesh)))

(defmacro with-mesh2d-trig-accessors
    ((mesh &key (x nil) (y nil)) &body body)
  (with-gensyms (i)
    (once-only (mesh)
      `(flet (,@(list1-if x `(,x (,i)
                                 (aref (mesh2d-trig-vertices ,mesh) ,i 0)))
              ,@(list1-if y `(,y (,i)
                                 (aref (mesh2d-trig-vertices ,mesh) ,i 1))))
         (declare (inline ,@(loop :for name :in (list x y)
                                  :if name :collect name)))
         ,@body))))

(defun mesh2d-trig-vertex-elt (mesh vertex-index)
  (with-mesh2d-trig-accessors (mesh :x x :y y)
    (let1 array (make-array 2 :element-type (mesh2d-trig-coordinate-type mesh))
      (setf (aref array 0) (x vertex-index))
      (setf (aref array 1) (y vertex-index))
      array)))

(defun mesh2d-trig-vise-elt (mesh vise-index)
  (with-slots (vises) mesh
    (loop
      :with array := (make-array 3
                                 :element-type (array-element-type vises))
      :for i :below 3
      :do (setf (aref array i) (aref vises vise-index i))
      :finally (return array))))

(defun mesh2d-trig-vertex-count (mesh)
  (with-slots (vertices) mesh
    (array-dimension vertices 0)))

(defun mesh2d-trig-vise-count (mesh)
  (with-slots (vises) mesh
    (array-dimension vises 0)))

(defun mesh2d-trig-vise->trig2d (mesh vise)
  (with-mesh2d-trig-accessors (mesh :x x :y y)
    (loop
      :with cv := (make-array
                   6
                   :element-type (mesh2d-trig-coordinate-type mesh))
      :for i :from 0
      :for vertex-index :across vise
      :do
         (setf (aref cv i) (x vertex-index))
         (setf (aref cv (+ i 3)) (y vertex-index))
      :finally
         (return cv))))

;;;
;;; unit square utility
;;;

(defun create-square-point-array
    (x-coordinates y-coordinates &key (element-type t))
  (loop
    :with nx :of-type fixnum := (length x-coordinates)
    :with ny :of-type fixnum := (length y-coordinates)
    :with array := (make-array `(,(* nx ny) 2)
                               :element-type element-type)
    :for j :of-type fixnum :from 0
    :for yj :across y-coordinates
    :do
       (loop
         :with offset :of-type fixnum := (* nx j)
         :for i :of-type fixnum :from 0
         :for xi :across x-coordinates
         :do
            (setf (aref array (+ offset i) 0) xi)
            (setf (aref array (+ offset i) 1) yj))
    :finally
       (return array)))

(defun create-square-vise-array (nx ny)
  (loop
    :with array := (make-array `(,(* 2 (1- nx) (1- ny)) 3)
                               :element-type 'fixnum)
    :for j :from 0 :to (- ny 2)
    :do
       (loop
         :for i :from 0 :to (- nx 2)
         :do
            ;;           (j(nx+1)+i)   (j*(nx+1)+i+1)
            ;; j+1                 *---*
            ;;                     |  /|
            ;; [tij = 2j(nx-1)+2i] | / | [2j*(nx-1)+2i+1]
            ;;                     |/  |
            ;; j                   *---*
            ;;        (pij = j*nx+i)   (j*nx+i+1)
            ;;
            ;;                     i  i+1
            (let* ((pij (+ (* j nx) i))
                   (tij (* 2 (- pij j))))
              ;; upper left triangle
              (setf (aref array tij 0) (+ pij nx))
              (setf (aref array tij 1) pij)
              (setf (aref array tij 2) (+ pij nx 1))
              ;; lower right triangle
              (setf (aref array (1+ tij) 0) (+ pij 1))
              (setf (aref array (1+ tij) 1) (+ pij nx 1))
              (setf (aref array (1+ tij) 2) pij)))
    :finally
       (return array)))

(defun mesh2d-unit-square (nx ny)
  (make-mesh2d-trig
   :vertices (create-square-point-array
              (vector-linspace 0d0 1d0 nx :element-type 'double-float)
              (vector-linspace 0d0 1d0 ny :element-type 'double-float)
              :element-type 'double-float)
   :vises (create-square-vise-array nx ny)))

;;; Local Variables:
;;; mode: lisp
;;; indent-tabs-mode: nil
;;; End:
