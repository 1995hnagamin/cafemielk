;;;; 2-D Triangle Geometry

(defpackage :cafemielk/geom/trig2d
  (:use :cl :cafemielk/util)
  (:export
   :trig2d-from-3points
   :trig2d-adherent-p
   :trig2d-area
   :trig2d-xref
   :trig2d-yref
   :trig2d-dx
   :trig2d-dy))
(in-package :cafemielk/geom/trig2d)

(defun 2d- (a b)
  "Returns A - B."
  (vector (- (aref a 0) (aref b 0))
          (- (aref a 1) (aref b 1))))

(defun ccw-p (a b c)
  (non-negative-p (cross2d (2d- b a)
                           (2d- c a))))

(defun trig2d-from-3points (a b c &key (element-type t))
  (flet ((make (a b c)
           (aref-let (((ax ay) a) ((bx by) b) ((cx cy) c))
             (make-array 6 :initial-contents (map 'vector
                                                  #'(lambda (z)
                                                      (coerce z element-type))
                                                  (vector ax bx cx ay by cy))
                           :element-type element-type
                           :adjustable nil))))
    (if (ccw-p a b c)
        (make a b c)
        (make a c b))))

(declaim (inline trig2d-xref))
(defun trig2d-xref (trig i)
  (aref trig i))

(declaim (inline trig2d-yref))
(defun trig2d-yref (trig i)
  (aref trig (+ i 3)))

(defmacro with-trig2d-accessors
    ((trig &key (x nil) (y nil) (dx nil) (dy nil) (dd nil))
     &body body)
  (with-gensyms (i j)
    (once-only (trig)
      `(flet (,@(list1-if  x `( ,x (,i) (trig2d-xref ,trig ,i)))
              ,@(list1-if  y `( ,y (,i) (trig2d-yref ,trig ,i)))
              ,@(list1-if dx `(,dx (,i ,j) (trig2d-dx ,trig ,i ,j)))
              ,@(list1-if dy `(,dy (,i ,j) (trig2d-dy ,trig ,i ,j)))
              ,@(list1-if dd `(,dd (,i ,j) `#(,(trig2d-dx ,trig ,i ,j)
                                              ,(trig2d-dy ,trig ,i ,j)))))
         (declare (inline ,@(loop :for name in (list x y dx dy dd)
                                  :if name :collect name)))
         ,@body))))

(declaim (inline trig2d-dx))
(defun trig2d-dx (trig i j)
  (declare (type (simple-array * (6)) trig))
  (with-trig2d-accessors (trig :x x)
    (- (x j) (x i))))

(declaim (inline trig2d-dy))
(defun trig2d-dy (trig i j)
  (with-trig2d-accessors (trig :y y)
    (- (y j) (y i))))

(declaim (inline trig2d-area))
(defun trig2d-area (trig)
  (declare (type (simple-array * (6)) trig))
  (with-trig2d-accessors (trig :dd dd)
    (/ (cross2d (dd 0 1) (dd 0 2)) 2)))

(defun trig2d-adherent-p (trig pt)
  (declare (type (simple-array * (6)) trig)
           (type (simple-array * (2)) pt))
  (with-trig2d-accessors (trig :x tx :y ty)
    (aref-let1 (px py) pt
      (flet ((check (j k)
             (non-negative-p
              (cross2d `#(,(- (tx j) px) ,(- (ty j) py))
                       `#(,(- (tx k) px) ,(- (ty k) py))))))
      (and (check 0 1)
           (check 1 2)
           (check 2 0))))))

;;; Local Variables:
;;; mode: lisp
;;; indent-tabs-mode: nil
;;; End:
