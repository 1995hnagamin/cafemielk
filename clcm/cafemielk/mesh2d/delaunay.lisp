;;;; Delaunay triangulation

(defpackage :cafemielk/mesh2d/delaunay
  (:use
   :cl
   :cafemielk/geom/trig2d
   :cafemielk/mesh2d/base
   :cafemielk/point-array
   :cafemielk/util)
  (:export
   :delaunay-triangulate))
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

(declaim (inline %history-dag-count))
(defun %history-dag-count (hdag)
  (declare (type %history-dag hdag)
           (values fixnum &optional))
  (length (%history-dag-vises hdag)))

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

(defun %history-dag-set-adjacency (hdag trig-index tr1 tr2 tr3)
  (declare (type %history-dag hdag)
           (type fixnum trig-index tr1 tr2 tr3))
  (let1 adj (aref (%history-dag-adjacent-trig-array hdag) trig-index)
    (setf (aref adj 0) tr1)
    (setf (aref adj 1) tr2)
    (setf (aref adj 2) tr3)))

(defun %history-dag-push-vise (v1 v2 v3 hdag)
  (declare (type fixnum v1 v2 v3)
           (type %history-dag hdag))
  (%history-dag-push (make-array '(3) :initial-contents `(,v1 ,v2 ,v3)
                                      :element-type 'fixnum)
                     hdag))

(defun %history-dag-add-child (hdag parent-index child-index)
  (declare (type %history-dag hdag)
           (type fixnum parent-index child-index))
  (if (%history-dag-children hdag parent-index)
      (vector-push child-index
                   (aref (%history-dag-children-array hdag) parent-index))
      (let1 array (make-array '(3) :element-type 'fixnum
                                   :adjustable t
                                   :fill-pointer 0)
        (vector-push child-index array)
        (setf (aref (%history-dag-children-array hdag) parent-index) array))))

(defun %find-leaf-containing-edge (hdag trig-index e1 e2)
  (declare (type %history-dag hdag)
           (type fixnum trig-index e1 e2)
           (values (or null fixnum) (or null fixnum) &optional))
  (labels ((search-node (tr)
             (declare (type fixnum tr))
             (if (%history-dag-leafp hdag tr)
                 (let1 k (- (reduce #'+ (%history-dag-vise hdag tr)) e1 e2)
                   (values tr k))
                 (loop
                   :for child :of-type fixnum
                     :across (%history-dag-children hdag tr)
                   :when (%opposite-vertex (%history-dag-vise hdag child) e1 e2)
                     :do (return (search-node child))
                   :finally
                      (return (values nil nil))))))
    (if (>= trig-index 0)
        (search-node trig-index)
        (values trig-index nil))))

(defun %adjacent-trig-index (hdag trig-index vertex-index)
  (declare (type fixnum trig-index vertex-index)
           (type %history-dag hdag)
           (values (or null fixnum)))
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

(defun %find-leaf-containing-vertex (vertex-index hdag point-array)
  (declare (type %history-dag hdag)
           (values fixnum &optional))
  (labels ((walk (trig-index)
             (declare (type fixnum trig-index))
             (if (%history-dag-leafp hdag trig-index)
                 trig-index
                 (loop
                   :for child-index
                     :across (%history-dag-children hdag trig-index)
                   :for child := (%history-dag-vise hdag child-index)
                   :if (%adherentp vertex-index child point-array) :do
                     (return (walk child-index))
                   :finally
                      (error "not found")))))
    (walk 0)))

(defun %find-adherent-edge (vertex-index trig-vise point-array)
  (declare (type fixnum vertex-index)
           (type (vertex-index-sequence 3) trig-vise)
           (type (point-array-2d * *) point-array)
           (values (or null fixnum) (or null fixnum) (or null fixnum)
                   &optional))
  (flet ((on-line-p (p i j)
           (declare (type fixnum p i j)
                    (values boolean &optional))
           (and (not (%counterclockwisep p i j point-array))
                (not (%clockwisep p i j point-array)))))
    (aref-let1 (vi vj vk) trig-vise
      (cond
        ((on-line-p vertex-index vj vk) (values vj vk vi))
        ((on-line-p vertex-index vk vi) (values vk vi vj))
        ((on-line-p vertex-index vi vj) (values vi vj vk))
        (t (values nil nil nil))))))

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

(defun %history-dag-remove-virtual-points (hdag)
  (declare (type %history-dag hdag))
  (loop
    :with array := (make-array 0 :fill-pointer 0 :adjustable t)
    :for i :of-type fixnum :below (%history-dag-count hdag)
    :for vise :of-type (vertex-index-sequence 3)
      := (%history-dag-vise hdag i)
    :when (and (%history-dag-leafp hdag i)
               (every #'non-negative-p vise))
      :do
         (vector-push-extend vise array)
    :finally
       (return array)))

(defmacro with-new-trigs (hdag bindings &body body)
  (once-only (hdag)
    (let* ((trig-names
             (mapcar (lambda (bind)
                       (destructuring-bind
                           (name &key vise parents adjacence) bind
                         (declare (ignorable name vise parents adjacence))
                         name))
                     bindings))
           (vises
             (mapcar (lambda (bind)
                       (destructuring-bind
                           (name &key vise parents adjacence) bind
                         (declare (ignorable name vise parents adjacence))
                         vise))
                     bindings))
           (parent-lists
             (mapcar (lambda (bind)
                       (destructuring-bind
                           (name &key vise parents adjacence) bind
                         (declare (ignorable name vise parents adjacence))
                         parents))
                     bindings))
           (adjacence-lists
             (mapcar (lambda (bind)
                       (destructuring-bind
                           (name &key vise parents adjacence) bind
                         (declare (ignorable name vise parents adjacence))
                         adjacence))
                     bindings))
           (parent-gensym-list
             (loop :for  parents :in parent-lists
                   :collect (loop :for parent :in parents
                                  :collect (gensym))))
           (adjacence-gensym-list
             (loop :for bind :in bindings
                   :collect (loop :repeat 3 :collect (gensym)))))
      `(let ,(loop :for name :in trig-names
                   :for vise :in vises
                   :collect `(,name (%history-dag-push-vise ,@vise hdag)))
         (let (,@(loop
                   :for adjacence-gensyms :in adjacence-gensym-list
                   :for adjacence-list :in adjacence-lists
                   :append (loop :for g :in adjacence-gensyms
                                 :for expr :in adjacence-list
                                 :collect `(,g ,expr)))
               ,@(loop
                   :for parent-gensyms :in parent-gensym-list
                   :for parents :in parent-lists
                   :append (loop :for g :in parent-gensyms
                                 :for parent :in parents
                                 :collect `(,g ,parent))))
           ,@(loop :for name :in trig-names
                   :for adjacence-gensyms :in adjacence-gensym-list
                   :collect `(%history-dag-set-adjacency ,hdag ,name
                                                         ,@adjacence-gensyms))
           ,@(loop :for name :in trig-names
                   :for parent-gensyms :in parent-gensym-list
                   :append (loop
                             :for g :in parent-gensyms
                             :collect `(%history-dag-add-child ,hdag ,g ,name)))
           ,@body)))))

;; Mark Berg, Otfried Cheong, Marc Kreveld, and Mark Overmars
;; _Computational Geometry: Algorithms and Applications_
(defun delaunay-triangulate (point-array)
  (declare (type (point-array-2d * *) point-array)
           (values (vector (vertex-index-sequence 3)) &optional)
           (optimize (speed 3)))
  (let* ((npoint (array-dimension point-array 0))
         (hdag (%create-empty-history-dag))
         (pzero (%highest-point-index point-array)))
    (declare (type %history-dag hdag)
             (type fixnum npoint pzero))
    (labels ((push-vise (i j k)
               (declare (type fixnum i j k))
               (assert (and (/= i j) (/= j k) (/= k i)))
               (assert (%counterclockwisep i j k point-array))
               (%history-dag-push-vise i j k hdag))
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
                       (ts k) (%adjacent-trig-index hdag tr r)
                   (when (not ts)
                     (return-from body))
                   (when (not (%legalp r i j k point-array))
                     (let* ((tsj (%adjacent-trig-index hdag ts j))
                            (trj (%adjacent-trig-index hdag tr j))
                            (tsi (%adjacent-trig-index hdag ts i))
                            (tri (%adjacent-trig-index hdag tr i))
                            (t1 (push-vise r i k))
                            (t2 (push-vise r k j)))
                       (%history-dag-set-adjacency hdag t1
                                                   tsj t2 trj)
                       (%history-dag-set-adjacency hdag t2
                                                   tsi tri t1)
                       (%history-dag-add-child hdag tr t1)
                       (%history-dag-add-child hdag tr t2)
                       (%history-dag-add-child hdag ts t1)
                       (%history-dag-add-child hdag ts t2)
                       (legalize-edge r i k t1)
                       (legalize-edge r k j t2)))))))
      (loop
        :initially
           (push-vise -2 -1 pzero)
           (%history-dag-set-adjacency hdag 0 -1002 -1001 -1000)
        :with indexes :of-type (simple-array fixnum (*))
          := (let ((indexes (iota-array npoint :element-type 'fixnum)))
               (declare (type (simple-array fixnum (*)) indexes))
               (rotatef (aref indexes 0) (aref indexes pzero))
               (fisher-yates-shuffle indexes 1 npoint))

        :for r-index :of-type fixnum :from 1 :below npoint
        :for r :of-type fixnum := (aref indexes r-index)
        :for tr :of-type fixnum
          := (%find-leaf-containing-vertex r hdag point-array)
        :for vise :of-type (vertex-index-sequence 3)
          := (%history-dag-vise hdag tr)
        :for (e1 e2 tr-opposite)
          := (multiple-value-list (%find-adherent-edge r vise point-array))
        :if e1 :do
          ;; Point R is on the edge (E1, E2) of Triangle TR
          (multiple-value-bind (ts ts-opposite)
              (%adjacent-trig-index hdag tr tr-opposite)
            (declare (type fixnum ts)) ;; TS must not be nil
            (if ts-opposite
                (let* ((tre1 (%adjacent-trig-index hdag tr e1))
                       (tre2 (%adjacent-trig-index hdag tr e2))
                       (tse1 (%adjacent-trig-index hdag ts e1))
                       (tse2 (%adjacent-trig-index hdag ts e2))
                       (t1 (push-vise r e2 tr-opposite))
                       (t2 (push-vise r tr-opposite e1))
                       (t3 (push-vise r e1 ts-opposite))
                       (t4 (push-vise r ts-opposite e2)))
                  (%history-dag-set-adjacency hdag t1
                                              tre1 t2 t4)
                  (%history-dag-set-adjacency hdag t2
                                              tre2 t3 t1)
                  (%history-dag-set-adjacency hdag t3
                                              tse2 t4 t2)
                  (%history-dag-set-adjacency hdag t4
                                              tse1 t1 t3)
                  (%history-dag-add-child hdag tr t1)
                  (%history-dag-add-child hdag tr t2)
                  (%history-dag-add-child hdag ts t3)
                  (%history-dag-add-child hdag ts t4)
                  (legalize-edge r e2 tr-opposite t1)
                  (legalize-edge r tr-opposite e1 t2)
                  (legalize-edge r e1 ts-opposite t3)
                  (legalize-edge r ts-opposite e2 t4))
                (let* ((tre1 (%adjacent-trig-index hdag tr e1))
                       (tre2 (%adjacent-trig-index hdag tr e2))
                       (t1 (push-vise r e2 tr-opposite))
                       (t2 (push-vise r tr-opposite e1)))
                  (%history-dag-set-adjacency hdag t1
                                              tre1 t2 ts)
                  (%history-dag-set-adjacency hdag t2
                                              tre2 ts t1)
                  (%history-dag-add-child hdag tr t1)
                  (%history-dag-add-child hdag tr t2)
                  (legalize-edge r e2 tr-opposite t1)
                  (legalize-edge r tr-opposite e1 t2))))
        :else :do
          ;; Point R is interior of Triangle TR
          (aref-let (((i j k) vise)
                     ((adi adj adk) (%history-dag-adjacent-trig hdag tr)))
            (declare (type fixnum i j k))
            ;; add edges r-i, r-j, r-k
            (let ((tr1 (push-vise r i j))
                  (tr2 (push-vise r j k))
                  (tr3 (push-vise r k i)))
              (declare (type fixnum tr1 tr2 tr3))
              (%history-dag-set-adjacency hdag tr1
                                          adk tr2 tr3)
              (%history-dag-set-adjacency hdag tr2
                                          adi tr3 tr1)
              (%history-dag-set-adjacency hdag tr3
                                          adj tr1 tr2)
              (%history-dag-add-child hdag tr tr1)
              (%history-dag-add-child hdag tr tr2)
              (%history-dag-add-child hdag tr tr3)
              ;; legalize edges
              (legalize-edge r i j tr1)
              (legalize-edge r j k tr2)
              (legalize-edge r k i tr3)))
        :end
        :finally
           (return (%history-dag-remove-virtual-points hdag))))))

;;; Local Variables:
;;; mode: lisp
;;; indent-tabs-mode: nil
;;; End:
