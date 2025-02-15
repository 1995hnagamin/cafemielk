;;;; 2-D Triangle Geometry

(defpackage :cafemielk/geom/trig2d
  (:use
   :cl
   :cafemielk/point-array
   :cafemielk/util)
  (:export
   :clockwisep
   :counterclockwisep
   :in-circle
   :in-circle-p
   :trig2d
   :trig2d-from-3points
   :trig2d-adherent-p
   :trig2d-area
   :trig2d-xref
   :trig2d-yref
   :trig2d-dx
   :trig2d-dy
   :with-trig2d-accessors))
(in-package :cafemielk/geom/trig2d)

(defun 2d- (a b)
  "Returns A - B."
  (declare (type (simple-array * (2)) a b)
           (values (simple-array * (2)) &optional))
  (vector (- (xref a) (xref b))
          (- (yref a) (yref b))))

(declaim (inline counterclockwisep))
(defun counterclockwisep (origin pivot target)
  (declare (type (simple-array * (2)) origin pivot target)
           (values boolean &optional))
  (plusp (cross2d (2d- pivot origin)
                  (2d- target origin))))

(declaim (inline clockwisep))
(defun clockwisep (origin pivot target)
  (declare (type (simple-array * (2)) origin pivot target)
           (values boolean &optional))
  (minusp (cross2d (2d- pivot origin)
                   (2d- target origin))))

(defmacro sum-square (&rest numbers)
  `(+ ,@(loop :for num :in numbers :collect `(expt ,num 2))))

(declaim (inline in-circle in-circle-p))
(defun in-circle (a b c target)
  (declare (type (simple-array * (2)) a b c target))
  (aref-let (((ax ay) (2d- a target))
             ((bx by) (2d- b target))
             ((cx cy) (2d- c target)))
    (+ (* ax by (sum-square cx cy))
       (* bx cy (sum-square ax ay))
       (* cx ay (sum-square bx by))
       (- (* ax cy (sum-square bx by)))
       (- (* bx ay (sum-square cx cy)))
       (- (* cx by (sum-square ax ay))))))

(defun in-circle-p (a b c target)
  (declare (type (simple-array * (2)) a b c target)
           (values boolean &optional))
  (plusp (in-circle a b c target)))

(deftype trig2d (&optional (element-type '*))
  `(point-array ,element-type 3 2))

(defun trig2d-from-3points (a b c &key (element-type t))
  (declare (values (trig2d)))
  (flet ((make (a b c)
           (aref-let (((ax ay) a) ((bx by) b) ((cx cy) c))
             (make-array '(3 2)
                         :initial-contents `((,(coerce ax element-type)
                                              ,(coerce ay element-type))
                                             (,(coerce bx element-type)
                                              ,(coerce by element-type))
                                             (,(coerce cx element-type)
                                              ,(coerce cy element-type)))
                         :element-type element-type
                         :adjustable nil))))
    (if (counterclockwisep a b c)
        (make a b c)
        (make a c b))))

(declaim (inline trig2d-xref))
(defun trig2d-xref (trig i)
  (declare (type (trig2d) trig))
  (aref trig i 0))

(declaim (inline trig2d-yref))
(defun trig2d-yref (trig i)
  (declare (type (trig2d) trig))
  (aref trig i 1))

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
  (declare (type (trig2d) trig))
  (with-trig2d-accessors (trig :x x)
    (- (x j) (x i))))

(declaim (inline trig2d-dy))
(defun trig2d-dy (trig i j)
  (declare (type (trig2d) trig))
  (with-trig2d-accessors (trig :y y)
    (- (y j) (y i))))

(declaim (inline trig2d-area))
(defun trig2d-area (trig)
  (declare (type (trig2d) trig))
  (with-trig2d-accessors (trig :dd dd)
    (/ (cross2d (dd 0 1) (dd 0 2)) 2)))

(defun trig2d-adherent-p (trig pt)
  (declare (type (trig2d) trig)
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
