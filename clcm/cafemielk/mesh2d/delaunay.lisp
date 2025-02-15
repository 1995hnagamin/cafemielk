;;;; Delaunay triangulation

(defpackage :cafemielk/mesh2d/delaunay
  (:use
   :cl
   :cafemielk/geom/trig2d
   :cafemielk/mesh2d/base
   :cafemielk/point-array
   :cafemielk/util)
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
  (declare (type (point-array-2d * *) point-array)
           (values fixnum &optional))
  (flet ((lex< (i j)
           (declare (type fixnum i j))
           (lexicographic< (i j)
               (k)
               ((point-aref-x point-array k))
               ((point-aref-y point-array k)))))
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
           (type (point-array-2d * *) point-array)
           (values boolean &optional))
  (cond
    ((= i -2) nil)
    ((= k -2) t)
    ((= k -1) nil)
    ((= i -1) t)
    (t (with-point-coords* (((xi yi) point-array i)
                            ((xk yk) point-array k))
         (cond
           ((< yi yk) t)
           ((< yk yi) nil)
           (t (< xi xk))))))) ; rightmost is the highest

(defun %counterclockwisep (r i j point-array)
  (declare (type fixnum r i j)
           (type (point-array-2d * *) point-array)
           (values boolean &optional))
  (labels ((point-ref (k)
             (declare (type fixnum k))
             (coord-vec-2d point-array k))
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
           (type (point-array-2d * *) point-array)
           (values boolean &optional))
  (labels ((point (k)
             (declare (type fixnum k))
             (coord-vec-2d point-array k))
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
           (type (vertex-index-sequence 3) trig-vise)
           (type (point-array-2d * *) point-array)
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
           (type (vertex-index-sequence 3) trig-vise)
           (type (point-array-2d * *) point-array)
           (values boolean &optional))
  (flet ((not-cw-p (r i j)
           (declare (type fixnum r i j))
           (not (%clockwisep r i j point-array))))
    (aref-let1 (i j k) trig-vise
      (and (not-cw-p r i j)
           (not-cw-p r j k)
           (not-cw-p r k i)))))

(defun %opposite-vertex (trig-vise v1 v2)
  "Returns the vertex opposite to the edge (V1,V2) in the triangle TRIG-VISE.
The edge must be oriented counterclockwise with respect to the triangle.
Returns NIL if V1 and V2 do not form a counterclockwise edge of the triangle.

     v3
    /  \\
   /    \\
v1 -----> v2"
  (declare (type fixnum v1 v2)
           (type (vertex-index-sequence 3) trig-vise)
           (values (or null fixnum) &optional))
  (aref-let1 (i j k) trig-vise
    (declare (type fixnum i j k))
    (cond
      ((and (= j v1) (= k v2)) i)
      ((and (= k v1) (= i v2)) j)
      ((and (= i v1) (= j v2)) k)
      (t nil))))

(defun %opposite-edge (trig-vise vertex-index)
  (declare (type fixnum vertex-index)
           (type (vertex-index-sequence 3) trig-vise)
           (values (or null fixnum) (or null fixnum) &optional))
  (aref-let1 (i j k) trig-vise
    (declare (type fixnum i j k))
    (cond
      ((= i vertex-index) (values j k))
      ((= j vertex-index) (values k i))
      ((= k vertex-index) (values i j))
      (t (values nil nil)))))


;; Point location structure for Delaunay triangulation
;; (Guibas et al., 1992)
(defstruct %history-dag
  ;; Vertex index sequences for each node
  (vises nil
   :type (array (vertex-index-sequence 3) (*)))
  ;; Child triangle indices for each node (nil for leaf nodes)
  (children-array nil
   :type (array (or null (array fixnum (3))) (*)))
  ;; Adjacent triangle indices for each node
  (adjacent-trig-array nil
   :type (array (simple-array fixnum (3)) (*))))

(defun %create-empty-history-dag ()
  (make-%history-dag
   :vises
   (make-array 0 :element-type '(vertex-index-sequence 3)
                 :adjustable t
                 :fill-pointer 0)
   :children-array
   (make-array 0 :element-type '(or null (array fixnum (3)))
                 :adjustable t
                 :fill-pointer 0)
   :adjacent-trig-array
   (make-array 0 :element-type '(array fixnum (3))
                 :adjustable t
                 :fill-pointer 0)))

(declaim (inline %history-dag-children))
(defun %history-dag-children (hdag trig-index)
  (aref (%history-dag-children-array hdag) trig-index))

(declaim (inline %history-dag-vise))
(defun %history-dag-vise (hdag trig-index)
  (declare (type fixnum trig-index)
           (type %history-dag hdag)
           (values (vertex-index-sequence 3) &optional))
  (aref (%history-dag-vises hdag) trig-index))

(declaim (inline %history-dag-adjacent-trig))
(defun %history-dag-adjacent-trig (hdag trig-index)
  (declare (type fixnum trig-index)
           (type %history-dag hdag)
           (values (simple-array fixnum (3)) &optional))
  (aref (%history-dag-adjacent-trig-array hdag) trig-index))

(defun %history-dag-leafp (hdag trig-index)
  (declare (type fixnum trig-index)
           (type %history-dag hdag)
           (values boolean))
  (null (%history-dag-children hdag trig-index)))

(defconstant +invalid-triangle-index+ -1000)
(defun %history-dag-push (vise hdag)
  (declare (type (vertex-index-sequence 3) vise)
           (type %history-dag hdag))
  (vector-push-extend vise (%history-dag-vises hdag))
  (vector-push-extend nil (%history-dag-children-array hdag))
  (vector-push-extend (make-array 3 :initial-element +invalid-triangle-index+
                                    :element-type 'fixnum)
                      (%history-dag-adjacent-trig-array hdag)))

(defun %find-leaf-containing-edge (hdag trig-index e1 e2)
  (declare (type %history-dag hdag)
           (type fixnum trig-index e1 e2)
           (values (or null fixnum) &optional))
  (labels ((search-node (tr)
             (declare (type fixnum tr))
             (if (%history-dag-leafp hdag tr)
                 tr
                 (loop
                   :for child :of-type fixnum
                     :across (%history-dag-children hdag tr)
                   :when (%opposite-vertex (%history-dag-vise hdag child) e1 e2)
                     :do (return (search-node child))
                   :finally
                      (return nil)))))
    (if (>= trig-index 0)
        (search-node trig-index)
        trig-index)))

(defun %adjacent-trig-index (hdag trig-index vertex-index)
  (declare (type fixnum trig-index vertex-index)
           (type %history-dag hdag))
  (let ((vise (%history-dag-vise hdag trig-index))
        (adj-trigs (%history-dag-adjacent-trig hdag trig-index)))
    (aref-let1 (vi vj vk) vise
      (cond
        ((= vi vertex-index)
         (%find-leaf-containing-edge hdag (aref adj-trigs 0) vk vj))
        ((= vj vertex-index)
         (%find-leaf-containing-edge hdag (aref adj-trigs 1) vi vk))
        ((= vk vertex-index)
         (%find-leaf-containing-edge hdag (aref adj-trigs 2) vj vi))
        (t (error "yee"))))))

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
           (type (point-array-2d * *) point-array)
           (values boolean &optional))
  (flet ((ccwp (r i j)
           (declare (type fixnum r i j))
           (%counterclockwisep r i j point-array))
         (point-ref (i)
           (declare (type fixnum i))
           (coord-vec-2d point-array i)))
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
  (declare (type (vector (vertex-index-sequence 3)) vises)
           (type (vector boolean) flags))
  (loop
    :with array := (make-array 0 :fill-pointer 0 :adjustable t)
    :for i :of-type fixnum :below (length vises)
    :for vise :of-type (vertex-index-sequence 3) := (aref vises i)
    :when (and (aref flags i)
               (every #'non-negative-p vise))
      :do
         (vector-push-extend vise array)
    :finally
       (return array)))

;; Mark Berg, Otfried Cheong, Marc Kreveld, and Mark Overmars
;; _Computational Geometry: Algorithms and Applications_
(defun delaunay-triangulate (point-array)
  (declare (type (point-array-2d * *) point-array)
           (values (vector (vertex-index-sequence 3)) &optional)
           (optimize (speed 3)))
  (let* ((npoint (array-dimension point-array 0))
         (vises (make-array 0 :element-type '(vertex-index-sequence 3)
                              :adjustable t
                              :fill-pointer 0))
         (flags (make-array 0 :element-type 'boolean
                              :adjustable t
                              :fill-pointer 0))
         (pzero (%highest-point-index point-array)))
    (declare (type (vector (vertex-index-sequence 3)) vises)
             (type (vector boolean) flags)
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
        :for vise :of-type (vertex-index-sequence 3) := (aref vises tr)
        :if (%innerp r vise point-array) :do
          (aref-let1 (i j k) vise
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
