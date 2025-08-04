;;;; 2D Bubble mesh method

(defpackage :cafemielk/mesh/bubble2d
  (:use
   :cl
   :cafemielk/point-array
   :cafemielk/util)
  (:export
   ))
(in-package :cafemielk/mesh/bubble2d)

(declaim (inline interbubble-force))
(defun interbubble-force (factor limit-length l)
  (let ((x (/ l limit-length)))
    (if (> x 3/2)
        0
        (* factor (+ (* 10 x x x) (* -19 x x) 9) 1/8))))

(declaim (inline sqrt-sum-square))
(defun sqrt-sum-square (x y)
  (sqrt (+ (* x x) (* y y))))

(defun bubble-distance (point-array i j)
  (declare (type (point-array-2d * *) point-array)
           (type fixnum i j))
  (sqrt-sum-square (- (point-aref-x point-array i)
                      (point-aref-x point-array j))
                   (- (point-aref-y point-array i)
                      (point-aref-y point-array j))))

(defun compute-force (point-array i j)
  (declare (type (point-array-2d * *) point-array)
           (type fixnum i j))
  )

#|
(defun relax-bubbles (point-array spacing-function)
  (declare (type (point-array-2d * *) point-array))
  (loop
    :with point-count := (point-array-count point-array)
    :with radii := (make-array `(,point-count))
    :with forces := (make-array `(,point-count))
    :repeat 10
    :do
       (loop
         :for i :below point-count
         :do
            (setf (aref radii i) (funcall spacing-function
                                          (point-aref-x point-array i)
                                          (point-aref-y point-array i))))
       (fill-array-with forces 0)
       (loop
         :for i :below point-count
         :do
            (loop
              :for j :below point-count
              :do
                 (incf ))
         )))

(defun fig8a-fun (x y)
  (declare (type double-float x y)
           (values double-float &optional))
  (let ((c1 5d0) (c2 0d0))
    (/ (+ c1 (sin (+ c2 (* x y))))))
  )
|#

;;; Local Variables:
;;; mode: lisp
;;; indent-tabs-mode: nil
;;; End:
