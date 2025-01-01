;;;; 2-D Triangle Geometry

(defpackage :cafemielk/geom/trig2d
  (:use :cl :cafemielk/util)
  (:export
   :trig2d-adherent-p
   :trig2d-area
   :trig2d-xref
   :trig2d-yref))
(in-package :cafemielk/geom/trig2d)


(declaim (inline trig2d-xref))
(defun trig2d-xref (trig i)
  (aref trig i))

(declaim (inline trig2d-yref))
(defun trig2d-yref (trig i)
  (aref trig (+ i 3)))

(defmacro with-x_&y_ (trig &body body)
  (with-gensyms (i j)
    (once-only (trig)
    `(flet ((x_ (,i) (trig2d-xref ,trig ,i))
            (y_ (,j) (trig2d-yref ,trig ,j)))
       (declare (inline x_ y_))
       (declare (ignorable #'x_ #'y_))
       ,@body))))

(declaim (inline trig2d-dx))
(defun trig2d-dx (trig i j)
  (declare (type (simple-array * (6)) trig))
  (with-x_&y_ trig
    (- (x_ j) (x_ i))))

(declaim (inline trig2d-dy))
(defun trig2d-dy (trig i j)
  (with-x_&y_ trig
    (- (y_ j) (y_ i))))

(declaim (inline trig2d-area))
(defun trig2d-area (trig)
  (declare (type (simple-array * (6)) trig))
  (flet ((dd (i j) `#(,(trig2d-dx trig i j)
                      ,(trig2d-dy trig i j))))
    (declare (inline dd))
    (/ (cross2d (dd 0 1) (dd 0 2)) 2)))

(defun trig2d-adherent-p (trig pt)
  (declare (type (simple-array * (6)) trig)
           (type (simple-array * (2)) pt))
  (with-x_&y_ trig
    (flet ((check (j k)
             (non-negative-p
              (cross2d `#(,(- (x_ j) (aref pt 0)) ,(- (y_ j) (aref pt 1)))
                       `#(,(- (x_ k) (aref pt 0)) ,(- (y_ k) (aref pt 1)))))))
      (and (check 0 1)
           (check 1 2)
           (check 2 0)))))
