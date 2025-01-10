(defpackage :cafemielk-tests/all
  (:use :cl :fiveam)
  (:local-nicknames (:cm :cafemielk))
  (:export :run-tests))

(in-package :cafemielk-tests/all)

(def-suite :cm-test-all)
(in-suite :cm-test-all)

;; WolframAlpha: `cross ((3,141,5),(92,65,35))`
(test test-cross3d
  (is (equalp #(4610 355 -12777)
              (cm:cross3d #(3 141 5) #(92 65 35)))))

;; WolframAlpha: `triangle (3,653) (141,589) (592,793)`
(test test-trig2d-area
  (is (= (cm:trig2d-area #(3 141 592 653 589 793))
         28508)))

(test test-rvd
  (let ((target (cm:make-csr
                 :nrow 10 :ncol 10
                 :rowptr (coerce #(0 0 1 3 4 4 4 4 4 4 4)
                                 '(simple-array fixnum (11)))
                 :colind (coerce #(0 1 9 3)
                                 '(simple-array fixnum (4)))
                 :entries (coerce #(50.0d0 21.0d0 29.0d0 33.0d0)
                                  '(simple-array double-float (4)))))
        (A (cm:create-empty-rvd 10 10 :element-type 'double-float)))
    (setf (cm:get-rvd A 3 3) 33.0d0)
    (incf (cm:get-rvd A 2 1) 21.0d0)
    (incf (cm:get-rvd A 2 9) 29.0d0)
    (incf (cm:get-rvd A 1 0) 10.0d0)
    (is (= 10.0d0 (cm:get-rvd A 1 0)))
    (incf (cm:get-rvd A 1 0) 40.0d0)
    (is (= 33.0d0 (cm:get-rvd A 3 3)))
    (is (= 50.0d0 (cm:get-rvd A 1 0)))
    (is (= 0.0d0 (cm:get-rvd A 8 8)))
    (is (= 2 (cm:rvd-row-count A 2)))
    (cm:rvd-delete! A 3 3)
    (is (= 0.0d0 (cm:get-rvd A 3 3)))
    (setf (cm:get-rvd A 3 3) 33.0d0)
    (is (equalp target
                (cm:rvd->csr A :element-type 'double-float)))))

(test test-dense-matrix
  (let ((A (cm:create-empty-dense-matrix 3 3)))
    (loop :for i :below 3 :do
      (loop :for j :below 3
            :do (setf (cm:dense-matrix-ref A i j) (* (1+ i) (1+ j)))))
    (is (equalp (cm:matrix-ref A 2 2) 9))))

(test test-coo
  (let ((entries (make-array 10 :adjustable t :fill-pointer 0))
        (rowind (make-array 10 :adjustable t :fill-pointer 0))
        (colind (make-array 10 :adjustable t :fill-pointer 0)))
    (vector-push-extend 1.25d0 entries)
    (vector-push-extend 0 rowind)
    (vector-push-extend 2 colind)
    ;; Check whether cm:make-coo correctly handles arrays with fill-pointers.
    (is (equalp (cm:make-csr :nrow 3 :ncol 3
                             :rowptr (coerce #(0 1 1 1)
                                             '(simple-array fixnum (4)))
                             :colind (coerce #(2) '(simple-array fixnum (1)))
                             :entries #(1.25d0))
                (cm:coo->csr (cm:make-coo :nrow 3 :ncol 3
                                          :rowind rowind
                                          :colind colind
                                          :entries entries))))))

;; / 1  2  0  0  0 \ / 5 \ = / 13 \
;; | 3  4  5  0  0 | | 4 |   | 46 |
;; | 0  6  7  8  0 | | 3 |   | 61 |
;; | 0  0  9 10 11 | | 2 |   | 58 |
;; \ 0  0  0 12 13 / \ 1 /   \ 37 /
(test test-csr-mv
  (let ((A (cm:coo->csr
            (cm:make-coo
             :nrow 5 :ncol 5
             :entries #(1 2 3 4 5 6 7 8 9 10 11 12 13)
             :rowind #(0 0 1 1 1 2 2 2 3 3 3 4 4)
             :colind #(0 1 0 1 2 1 2 3 2 3 4 3 4))))
        (x #(5 4 3 2 1)))
    (is (equalp (cm:mv A x) #(13 46 61 58 37)))))

(test test-create-square
  (is (equalp #2a((0 7) (1 7) (2 7)
                  (0 8) (1 8) (2 8)
                  (0 9) (1 9) (2 9))
              (cm:create-square-point-array #(0 1 2)
                                            #(7 8 9)
                                            :element-type 'fixnum)))
  (is (equalp #2a((3 0 4) (1 4 0) (4 1 5) (2 5 1)
                  (6 3 7) (4 7 3) (7 4 8) (5 8 4))
              (cm:create-square-vise-array 3 3)))
  (let ((mesh (cm:mesh2d-unit-square 5 5)))
    (is (= 25 (cm:mesh2d-trig-vertex-count mesh)))
    (is (equalp #(0.5d0 0.5d0)
                (cm:mesh2d-trig-vertex-elt mesh 12)))
    (is (equalp #(5 0 6)
                (cm:mesh2d-trig-vise-elt mesh 0)))
    (is (equalp #(  0   0 1/4
                  1/4   0 1/4)
                (cm:mesh2d-trig-vise->trig2d mesh #(5 0 6))))))

;;; Local Variables:
;;; mode: lisp
;;; indent-tabs-mode: nil
;;; End:
