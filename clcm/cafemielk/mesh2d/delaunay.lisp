;;;; Delaunay triangulation

(defpackage :cafemielk/mesh2d/delaunay
  (:use :cl :cafemielk/geom/trig2d :cafemielk/util)
  (:export
   ))
(in-package :cafemielk/mesh2d/delaunay)

(defun square (nx ny &key (element-type t))
  (loop
    :with array := (make-array `(,(* (1+ nx) (1+ ny)) 2)
                               :element-type element-type)
    :for j :from 0 :to ny
    :do
       (loop
         :for i :from 0 :to nx
         :do
            (setf (aref array (+ (* (1+ nx) j) i) 0)
                  (coerce (/ i nx) element-type))
            (setf (aref array (+ (* (1+ nx) j) i) 1)
                  (coerce (/ j ny) element-type)))
    :finally
       (return array)))

(defun random-range (start end)
  (+ start (random (- end start))))

(defun fisher-yates-shuffle (array start end)
  (loop
    :for i :downfrom (1- end) :above start
    :for j := (random-range start (1+ i))
    :do
       (rotatef (aref array i) (aref array j))
    :finally
       (return array)))

(declaim (inline iota-array))
(defun iota-array (count &key (start 0) (step 1) (element-type t))
  (loop
    :with array := (make-array `(,count)
                               :element-type element-type)
    :for i :from 0 :below count
    :do
       (setf (aref array i) (+ start (* i step)))
    :finally
       (return array)))

(defun copy-vector-adjustable (vector)
  (let ((length (length vector)))
    (make-array `(,length)
                :initial-contents vector
                :adjustable t
                :fill-pointer length)))

(defun delete-nth (n vector)
  (loop
    :for i :from (1+ n) :below (length vector)
    :do
       (rotatef (aref vector (1- i)) (aref vector i))
    :finally
       (vector-pop vector)))

(declaim (inline point-array-xref))
(defun point-array-xref (point-array i)
  (aref point-array i 0))

(declaim (inline point-array-yref))
(defun point-array-yref (point-array i)
  (aref point-array i 1))

(defun vise->trig (point-array vise)
  `#(,(point-array-xref point-array (aref vise 0))
     ,(point-array-xref point-array (aref vise 1))
     ,(point-array-xref point-array (aref vise 2))
     ,(point-array-yref point-array (aref vise 0))
     ,(point-array-yref point-array (aref vise 1))
     ,(point-array-yref point-array (aref vise 2))))

(declaim (inline point-array-count))
(defun point-array-count (point-array)
  (array-dimension point-array 0))

(declaim (inline point-array-nth))
(defun point-array-nth (n point-array)
  (declare (type fixnum n)
           (type (array * (* *)) point-array))
  `#(,(point-array-xref point-array n)
     ,(point-array-yref point-array n)))

(defmacro lexicographic< ((i j) (param) &rest clauses)
  (once-only (i j)
    (reduce
     #'(lambda (clause tail)
         (destructuring-bind (form &key (less '<) (equ '=)) clause
           (with-gensyms (form-* form-i form-j)
             `(flet ((,form-* (,param) ,form))
                (declare (inline ,form-*))
                (let ((,form-i (,form-* ,i))
                      (,form-j (,form-* ,j)))
                  (or (,less ,form-i ,form-j)
                      (and (,equ ,form-i ,form-j)
                           ,tail)))))))
     clauses
     :from-end t
     :start 0 :end (length clauses)
     :initial-value nil)))

(defun get-shuffled-indexes (point-array)
  (declare (type (array * (* *)) point-array))
  (flet ((lex< (i j)
           (declare (type fixnum i j))
           (lexicographic< (i j)
               (k)
               ((point-array-yref point-array k))
               ((point-array-xref point-array k)))))
    (let* ((npoint (point-array-count point-array))
           (indexes (iota-array npoint :element-type 'fixnum))
           (highest-point-index (loop
                                  :with m := 0
                                  :for i :from 1 :below npoint
                                  :when (lex< m i)
                                    :do (setf m i)
                                  :finally
                                     (return m))))
      (declare (type (array fixnum (*)) indexes)
               (type fixnum npoint))
      (rotatef (aref indexes 0) (aref indexes highest-point-index))
      (fisher-yates-shuffle indexes 1 npoint))))

(declaim (inline min&max))
(defun min&max (x y)
  (if (< x y)
      (values x y)
      (values y x)))

(defun delaunay-lex< (i k &key point-array)
  (declare (type fixnum i k))
  (cond
    ((= i -2) nil)
    ((= k -2) t)
    ((= k -1) nil)
    ((= i -1) t)
    (t (aref-let (((xi yi) (point-array-nth i point-array))
                  ((xk yk) (point-array-nth k point-array)))
         (cond
           ((< yi yk) t)
           ((< yk yi) nil)
           (t (< xi xk))))))) ; rightmost is the highest

(defun delaunay-ccw-p (r i j &key point-array)
  (declare (type fixnum r i j))
  (labels ((point-ref (k)
             (declare (type fixnum k))
             (point-array-nth k point-array))
           (ccwp (r i j)
             (cond
               ((= i -2) (delaunay-lex< j r :point-array point-array))
               ((= j -1) (delaunay-lex< i r :point-array point-array))
               (t (ccwp i j r)))))
    (if (and (>= r 0) (>= i 0) (>= j 0))
        ;; Normal case
        (counterclockwisep (point-ref r) (point-ref i) (point-ref j))
        ;; Parameters involve at least one virtual point
        (ccwp r i j))))

(defun delaunay-inner-p (r trig-vise point-array)
  (aref-let1 (i j k) trig-vise
    (and (delaunay-ccw-p r i j :point-array point-array)
         (delaunay-ccw-p r j k :point-array point-array)
         (delaunay-ccw-p r k i :point-array point-array))))

(defun delaunay-find-trig (r vises flags point-array)
  (loop
    :for vise-index :from 0 :below (length vises)
    :for vise := (aref vises vise-index)
    :when (and (elt flags vise-index)
               (delaunay-inner-p r vise point-array))
      :do
         (return vise-index)
    :finally
       (error "not found")))

;; Mark Berg, Otfried Cheong, Marc Kreveld, and Mark Overmars
;; _Computational Geometry: Algorithms and Applications_
(defun delaunay-triangulate (point-array)
  (let* ((npoint (array-dimension point-array 0))
         (vises (make-array 0 :adjustable t :fill-pointer 0))
         (flags (make-array 0 :adjustable t :fill-pointer 0))
         (indexes (get-shuffled-indexes point-array))
         (pzero (aref indexes 0))
         (super-trig `#(-2 -1 ,pzero)))
    (labels ((push-vise (vise)
               (aref-let1 (i j k) vise
                 (assert (and (/= i j) (/= j k) (/= k i)))
                 (assert (ccwp i j k)))
               (vector-push-extend vise vises)
               (vector-push-extend t flags))
             (nullify-vise (vise-index)
               (setf (aref flags vise-index) nil))
             (point-ref (i)
               (point-array-nth i point-array))
             (bounding-point-p (i)
               (declare (type fixnum i))
               (or (= i -2) (= i -1) (= i pzero)))
             (ccwp (r i j)
               (declare (type fixnum r i j))
               (delaunay-ccw-p r i j :point-array point-array))
             (adherent-p (r vise)
               (delaunay-inner-p r vise point-array))
             (find-trig (r)
               (delaunay-find-trig r vises flags point-array))
             (find-adjacent-trig (i j tr)
               (loop
                 :for ti :from 0 :below (length vises)
                 :when (and
                        (/= ti tr)
                        (elt flags ti)
                        (find i (aref vises ti))
                        (find j (aref vises ti)))
                   :do
                      (return
                        (values ti
                                (- (reduce #'+ (aref vises ti)) i j)))
                 :finally
                    (error "adjacent triangle not found")))
             (legalp (r i j k)
               (cond
                 ;; If not flippable, return t
                 ((not (ccwp r i k)) t)
                 ((not (ccwp r k j)) t)
                 ;; Normal case: check if k is outside circumcircle
                 ((every #'non-negative-p `#(,r ,i ,j ,k))
                  (not (in-circle-p (point-ref r)
                                    (point-ref i)
                                    (point-ref j)
                                    (point-ref k))))
                 (t (< (min k r) (min i j)))))
             (legalize-edge (r i j tr)
               (block body
                 (when (and (bounding-point-p i) (bounding-point-p j))
                   ;; Super-triangle edges are not flippable
                   (return-from body))
                 (multiple-value-bind (ts k) (find-adjacent-trig i j tr)
                   (when (not (legalp r i j k))
                     (nullify-vise tr)
                     (nullify-vise ts)
                     (let ((t1 (push-vise `#(,r ,i ,k)))
                           (t2 (push-vise `#(,r ,k ,j))))
                       (legalize-edge r i k t1)
                       (legalize-edge r k j t2)))))))
      (push-vise super-trig)
      (loop
        :for r-index :from 1 :below npoint
        :for r := (aref indexes r-index)
        :for tr := (find-trig r)
        :if (adherent-p r (aref vises tr)) :do
          (aref-let1 (i j k) (aref vises tr)
            (nullify-vise tr)
            ;; add edges r-i, r-j, r-k
            (let ((tr1 (push-vise `#(,r ,i ,j)))
                  (tr2 (push-vise `#(,r ,j ,k)))
                  (tr3 (push-vise `#(,r ,k ,i))))
              ;; legalize edges
              (legalize-edge r i j tr1)
              (legalize-edge r j k tr2)
              (legalize-edge r k i tr3)))
        :else :do
          (error "not implemented")
        :end)
      (loop
        :with array := (make-array 0 :fill-pointer 0 :adjustable t)
        :for i :below (length vises)
        :when (and (aref flags i) (every #'non-negative-p (aref vises i))) :do
          (vector-push-extend (aref vises i) array)
        :finally
           (return array)))))

;;; Local Variables:
;;; mode: lisp
;;; indent-tabs-mode: nil
;;; End:
