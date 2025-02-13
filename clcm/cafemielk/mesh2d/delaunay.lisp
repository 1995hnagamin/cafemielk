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
  (declare (type fixnum start end))
  (+ start (random (- end start))))

(defun fisher-yates-shuffle (array start end)
  (declare (type array array)
           (type fixnum start end))
  (loop
    :for i :of-type fixnum :downfrom (1- end) :above start
    :for j :of-type fixnum := (random-range start (1+ i))
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
  (declare (type fixnum i)
           (type (simple-array * (* 2)) point-array))
  (aref point-array i 0))

(declaim (inline point-array-yref))
(defun point-array-yref (point-array i)
  (declare (type fixnum i)
           (type (simple-array * (* 2)) point-array))
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
  (declare (type (simple-array * (* 2)) point-array))
  (array-dimension point-array 0))

(declaim (inline point-array-nth))
(defun point-array-nth (n point-array)
  (declare (type fixnum n)
           (type (simple-array * (* 2)) point-array))
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

(defun %highest-point-index (point-array)
  (declare (type (simple-array * (* 2)) point-array)
           (values fixnum &optional))
  (flet ((lex< (i j)
           (declare (type fixnum i j))
           (lexicographic< (i j)
               (k)
               ((point-array-yref point-array k))
               ((point-array-xref point-array k)))))
    (loop
      :with m :of-type fixnum := 0
      :for i :of-type fixnum :from 1 :below (point-array-count point-array)
      :when (lex< m i)
        :do (setf m i)
      :finally
         (return m))))

(declaim (inline min&max))
(defun min&max (x y)
  (if (< x y)
      (values x y)
      (values y x)))

(defun %lex< (i k point-array)
  (declare (type fixnum i k)
           (type (simple-array * (* 2)) point-array)
           (values boolean &optional))
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

(defun %counterclockwisep (r i j point-array)
  (declare (type fixnum r i j)
           (type (simple-array * (* 2)) point-array)
           (values boolean &optional))
  (labels ((point-ref (k)
             (declare (type fixnum k))
             (point-array-nth k point-array))
           (lex< (a b)
             (declare (type fixnum a b))
             (%lex< a b point-array)))
    (if (and (>= r 0) (>= i 0) (>= j 0))
        ;; Normal case
        (counterclockwisep (point-ref r) (point-ref i) (point-ref j))
        ;; Parameters involve at least one virtual point
        (let/goer ((r r) (i i) (j j)) go-loop
          (declare (type fixnum r i j))
          (cond
            ((= i -2) (lex< j r))
            ((= j -1) (lex< i r))
            (t (go-loop i j r)))))))

(defun %clockwisep (r i j point-array)
  (declare (type fixnum r i j)
           (type (array * (* 2)) point-array)
           (values boolean &optional))
  (labels ((point (k)
             (declare (type fixnum k))
             (point-array-nth k point-array))
           (lex< (a b)
             (declare (type fixnum a b))
             (%lex< a b point-array)))
    (if (and (>= r 0) (>= i 0) (>= j 0))
        ;; Normal case
        (clockwisep (point r) (point i) (point j))
        ;; Parameters involve at least one virtual point
        (let/goer ((r r) (i i) (j j)) go-loop
          (declare (type fixnum r i j))
          (cond
            ((= i -2) (lex< r j))
            ((= j -1) (lex< r i))
            (t (go-loop i j r)))))))

(defun %innerp (r trig-vise point-array)
  (declare (type fixnum r)
           (type (simple-array fixnum (3)) trig-vise)
           (type (simple-array * (* 2)) point-array)
           (values boolean &optional))
  (flet ((ccwp (r i j)
           (declare (type fixnum r i j))
           (%counterclockwisep r i j point-array)))
    (aref-let1 (i j k) trig-vise
      (declare (type fixnum i j k))
      (and (ccwp r i j)
           (ccwp r j k)
           (ccwp r k i)))))

(defun %adherentp (r trig-vise point-array)
  (declare (type fixnum r)
           (type (simple-array fixnum (3)) trig-vise)
           (type (simple-array * (* 2)) point-array)
           (values boolean &optional))
  (flet ((not-cw-p (r i j)
           (declare (type fixnum r i j))
           (not (%clockwisep r i j point-array))))
    (aref-let1 (i j k) trig-vise
      (and (not-cw-p r i j)
           (not-cw-p r j k)
           (not-cw-p r k i)))))

(defun %opposite-vertex (trig-vise e1 e2)
  (declare (type fixnum e1 e2)
           (type (simple-array fixnum (3)) trig-vise)
           (values (or null fixnum) &optional))
  (aref-let1 (i j k) trig-vise
    (declare (type fixnum i j k))
    (cond
      ((and (= j e1) (= k e2)) i)
      ((and (= k e1) (= i e2)) j)
      ((and (= i e1) (= j e2)) k)
      (t nil))))

(defun %opposite-edge (trig-vise vertex-index)
  (declare (type fixnum vertex-index)
           (type (simple-array fixnum (3)) trig-vise)
           (values (or null fixnum) (or null fixnum) &optional))
  (aref-let1 (i j k) trig-vise
    (declare (type fixnum i j k))
    (cond
      ((= i vertex-index) (values j k))
      ((= j vertex-index) (values k i))
      ((= k vertex-index) (values i j))
      (t (values nil nil)))))

(defun %find-trig (r vises flags point-array)
  (loop
    :for vise-index :from 0 :below (length vises)
    :for vise := (aref vises vise-index)
    :when (and (elt flags vise-index)
               (%adherentp r vise point-array))
      :do
         (return vise-index)
    :finally
       (error "not found")))

(defun %find-adjacent-trig (i j tr vises flags)
  (loop
    :for ti :from 0 :below (length vises)
    :when (and (/= ti tr)
               (elt flags ti)
               (find i (aref vises ti))
               (find j (aref vises ti)))
      :do
         (let1 k (- (reduce #'+ (aref vises ti)) i j)
           (return (values ti k)))
    :finally
       (error "adjacent triangle not found")))

(defun %legalp (r i j k point-array)
  (declare (type fixnum r i j k)
           (type (simple-array * (* 2)) point-array)
           (values boolean &optional))
  (flet ((ccwp (r i j)
           (declare (type fixnum r i j))
           (%counterclockwisep r i j point-array))
         (point-ref (i)
           (declare (type fixnum i))
           (point-array-nth i point-array)))
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
      (t (< (min k r) (min i j))))))

(defun %remove-virtual-points (vises flags)
  (declare (type (array (simple-array fixnum (3)) (*)) vises)
           (type (array boolean (*)) flags))
  (loop
    :with array := (make-array 0 :fill-pointer 0 :adjustable t)
    :for i :of-type fixnum :below (length vises)
    :for vise :of-type (simple-array fixnum (3)) := (aref vises i)
    :when (and (aref flags i)
               (every #'non-negative-p vise))
      :do
         (vector-push-extend vise array)
    :finally
       (return array)))

;; Mark Berg, Otfried Cheong, Marc Kreveld, and Mark Overmars
;; _Computational Geometry: Algorithms and Applications_
(defun delaunay-triangulate (point-array)
  (declare (type (simple-array * (* 2)) point-array))
  (let* ((npoint (array-dimension point-array 0))
         (vises (make-array 0 :adjustable t :fill-pointer 0))
         (flags (make-array 0 :adjustable t :fill-pointer 0))
         (pzero (%highest-point-index point-array)))
    (declare (type (array (simple-array * (3)) (*)) vises)
             (type (array boolean (*)) flags)
             (type fixnum npoint pzero))
    (labels ((push-vise (i j k)
               (declare (type fixnum i j k))
               (assert (and (/= i j) (/= j k) (/= k i)))
               (assert (%counterclockwisep i j k point-array))
               (vector-push-extend
                (make-array 3 :initial-contents `(,i ,j ,k)
                              :element-type 'fixnum)
                vises)
               (vector-push-extend t flags))
             (nullify-vise (vise-index)
               (setf (aref flags vise-index) nil))
             (bounding-point-p (i)
               (declare (type fixnum i))
               (or (= i -2) (= i -1) (= i pzero)))
             (legalize-edge (r i j tr)
               (declare (type fixnum r i j tr))
               (block body
                 (when (and (bounding-point-p i)
                            (bounding-point-p j))
                   ;; Super-triangle edges are not flippable
                   (return-from body))
                 (multiple-value-bind
                       (ts k) (%find-adjacent-trig i j tr vises flags)
                   (declare (type fixnum ts k))
                   (when (not (%legalp r i j k point-array))
                     (nullify-vise tr)
                     (nullify-vise ts)
                     (let ((t1 (push-vise r i k))
                           (t2 (push-vise r k j)))
                       (legalize-edge r i k t1)
                       (legalize-edge r k j t2)))))))
      (loop
        :initially
           (push-vise -2 -1 pzero)
        :with indexes :of-type (simple-array fixnum (*))
          := (let ((indexes (iota-array npoint :element-type 'fixnum)))
               (declare (type (simple-array fixnum (*)) indexes))
               (rotatef (aref indexes 0) (aref indexes pzero))
               (fisher-yates-shuffle indexes 1 npoint))

        :for r-index :of-type fixnum :from 1 :below npoint
        :for r :of-type fixnum := (aref indexes r-index)
        :for tr :of-type fixnum := (%find-trig r vises flags point-array)
        :if (%innerp r (aref vises tr) point-array) :do
          (aref-let1 (i j k) (aref vises tr)
            (declare (type fixnum i j k))
            (nullify-vise tr)
            ;; add edges r-i, r-j, r-k
            (let ((tr1 (push-vise r i j))
                  (tr2 (push-vise r j k))
                  (tr3 (push-vise r k i)))
              ;; legalize edges
              (legalize-edge r i j tr1)
              (legalize-edge r j k tr2)
              (legalize-edge r k i tr3)))
        :else :do
          (error "not implemented")
        :end
        :finally
           (return (%remove-virtual-points vises flags))))))

;;; Local Variables:
;;; mode: lisp
;;; indent-tabs-mode: nil
;;; End:
