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
    (incf (cm:get-rvd A 1 0) 40.0d0)
    (is (= 33.0d0 (cm:get-rvd A 3 3)))
    (is (= 50.0d0 (cm:get-rvd A 1 0)))
    (is (= 2 (cm:rvd-row-count A 2)))
    (is (equalp target
                (cm:rvd->csr A :element-type 'double-float)))))

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
