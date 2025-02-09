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
               (format t "(push-vise ~a)~%" vise)
               (aref-let (((i j k) vise))
                 (assert (and (/= i j) (/= j k) (/= k i)))
                 (assert (ccwp i j k)))
               (vector-push-extend vise vises)
               (vector-push-extend t flags))
             (nullify-vise (vise-index)
               (format t "(nullify-vise ~a[~a:~a])~%"
                       vise-index
                       (aref vises vise-index) (aref flags vise-index))
                   (setf (aref flags vise-index) nil))
             (point-ref (i)
               (point-array-nth i point-array))
             (bounding-point-p (i)
               (declare (type fixnum i))
               (or (= i -2) (= i -1) (= i pzero)))
             (lex< (i k)
               (declare (type fixnum i k))
               (format t "(lex< ~a ~a)~%" i k)
               (cond
                 ((= k -2) t)
                 ((= i -2) nil)
                 ((= i -1) t)
                 ((= k -1) nil)
                 (t (aref-let (((xi yi) (point-ref i))
                               ((xk yk) (point-ref k)))
                      (cond
                        ((< yi yk) t)
                        ((< yk yi) nil)
                        (t (> xi xk)))))))
             (ccwp (r i j)
               (declare (type fixnum r i j))
               (format t "(ccwp ~a ~a ~a)~%" r i j)
               (cond
                 ((and (>= r 0) (>= i 0) (>= j 0))
                  (counterclockwisep (point-ref r) (point-ref i) (point-ref j)))
                 ((= i -2) (lex< j r))
                 ((= j -1) (lex< i r))
                 (t (ccwp i j r))))
             (adherent-p (r vise)
               (format t "(adherent-p ~a[~a] ~a)~%"
                       r (point-ref r) vise)
               (aref-let (((i j k) vise))
                 (and (ccwp r i j) (ccwp r j k) (ccwp r k i))))
             (find-trig (r)
               (format t "(find-trig ~a[~a])~%" r (point-ref r))
               (loop
                 :for tr :from 0 :below (length vises)
                 :for trig := (aref vises tr)
                 :when (and (elt flags tr) (adherent-p r trig))
                   :do
                      (return tr)
                 :finally
                    (error "not found")))
             (find-adjoint (i j tr)
               (format t "(find-adjoint ~a[~a] ~a[~a] ~a[~a:~a])~%"
                       i (if (>= i 0) (point-ref i) "---")
                       j (if (>= j 0) (point-ref j) "---")
                       tr (aref vises tr) (aref flags tr))
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
                    (error "adjoint point not found")))
             (legalp (r i j tr)
               (format t "(legalp ~a[~a] ~a[~a] ~a[~a] ~a[~a])~%"
                       r (if (>= r 0) (point-ref r) "---")
                       i (if (>= i 0) (point-ref i) "---")
                       j (if (>= j 0) (point-ref j) "---")
                       tr (aref vises tr))
               (if (and (bounding-point-p i) (bounding-point-p j))
                   (values t nil nil)
                   (multiple-value-bind (ts k) (find-adjoint i j tr)
                     (format t "found adjoint: ~a[~a] ~a[~a]~%"
                             ts (aref vises ts)
                             k (if (>= k 0) (point-ref k) "---"))
                     (if (and (>= r 0) (>= i 0) (>= j 0) (>= k 0))
                         (values
                          (in-circle-p (point-array-nth r point-array)
                                       (point-array-nth i point-array)
                                       (point-array-nth j point-array)
                                       (point-array-nth k point-array))
                          ts k)
                         (values (< (min k r) (min i j))
                                 ts k)))))
             (legalize-edge (r i j tr)
               (sleep 0.5)
               (format t "(legalize-edge ~a[~a] ~a[~a] ~a[~a] ~a[~a:~a])~%"
                       r (if (>= r 0) (point-ref r) "---")
                       i (if (>= i 0) (point-ref i) "---")
                       j (if (>= j 0) (point-ref j) "---")
                       tr (aref vises tr) (aref flags tr))
               (multiple-value-bind (legal ts k) (legalp r i j tr)
                 (when legal
                   (format t "    legal. no operations needed.~%"))
                 (when (not legal)
                   (format t "    illegal~%")
                   (nullify-vise tr)
                   (nullify-vise ts)
                   (let ((t1 (push-vise `#(,r ,i ,k)))
                         (t2 (push-vise `#(,r ,k ,j))))
                     (legalize-edge r i k t1)
                     (legalize-edge r k j t2))))))
      (push-vise super-trig)
      (loop
        :initially (format t "loop: indexes: ~a~%" indexes)
        :for r-index :from 1 :below npoint
        :for r := (aref indexes r-index)
        :for tr := (find-trig r)
        :do
           (format t "loop: r = ~a~%" r)
        :if (adherent-p r (aref vises tr)) :do
          (format t "~a~%" (aref vises tr))
          (aref-let (((i j k) (aref vises tr)))
            (format t "i: ~a, j: ~a, k: ~a~%" i j k)
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
