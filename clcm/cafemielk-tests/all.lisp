(defpackage :cafemielk-tests/all
  (:use :cl)
  (:local-nicknames (:cm :cafemielk))
  (:export :run-tests))

(in-package :cafemielk-tests/all)

;; WolframAlpha: `cross ((3,141,5),(92,65,35))`
(defun test-cross3d ()
  (equalp (cm:cross3d #(3 141 5) #(92 65 35))
          #(4610 355 -12777)))

;; WolframAlpha: `triangle (3,653) (141,589) (592,793)`
(defun test-trig2d-area ()
  (= (cm:trig2d-area #(3 141 592 653 589 793))
     28508))

(defun test-rvd ()
  (let ((A (cm:create-empty-rvd 10 10 :element-type 'double-float)))
    (setf (cm:get-rvd A 3 3) 33.0d0)
    (incf (cm:get-rvd A 1 0) 10.0d0)
    (incf (cm:get-rvd A 1 0) 40.0d0)
    (and
     (= (cm:get-rvd A 3 3) 33.0d0)
     (= (cm:get-rvd A 1 0) 50.0d0))))

;; / 1  2  0  0  0 \ / 5 \ = / 13 \
;; | 3  4  5  0  0 | | 4 |   | 46 |
;; | 0  6  7  8  0 | | 3 |   | 61 |
;; | 0  0  9 10 11 | | 2 |   | 58 |
;; \ 0  0  0 12 13 / \ 1 /   \ 37 /
(defun test-csr-mv ()
  (let ((A (cm:coo->csr
            (cm:make-coo
             :nrow 5 :ncol 5
             :entries #(1 2 3 4 5 6 7 8 9 10 11 12 13)
             :rowind #(0 0 1 1 1 2 2 2 3 3 3 4 4)
             :colind #(0 1 0 1 2 1 2 3 2 3 4 3 4))))
        (x #(5 4 3 2 1)))
    (equalp (cm:mv A x) #(13 46 61 58 37))))

(defmacro report-tests (&rest test-exprs)
  `(progn
     ,@(loop :for expr :in test-exprs
             :collect `(format t "TEST ~a: ~:[FAIL~;PASS~]~%"
                               ',expr
                               ,expr))))

(defun run-tests ()
  (format t "Running Cafemielk tests...~%")
  (report-tests
   (test-cross3d)
   (test-trig2d-area)
   (test-rvd)
   (test-csr-mv)))
