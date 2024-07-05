;;;
;;; Test cafemielk
;;;

(use gauche.test)
(use srfi-133)

(test-start "cafemielk")
(use cafemielk)
(test-module 'cafemielk)

(test*
 "test-cafemielk"
 #(b c d)
 (vview->vector (make-vview #(a b c d) 1 #(3))))

;; WolframAlpha: `cross ((3,141,5),(92,65,35))`
(test*
 "test-cross3d"
 #(4610 355 -12777)
 (cross3d #(3 141 5) #(92 65 35)))

;; WolframAlpha: `triangle (3,653) (141,589) (592,793)`
(test*
 "test-trig2d-area"
 28508
 (trig2d-area #(3 141 592 653 589 793)))

(test*
 "test-mesh2d-unit-square"
 143
 (mesh2d-vertices-length (mesh2d-unit-square 11 13)))

;; / 1  2  0  0  0 \ / 5 \ = / 13 \
;; | 3  4  5  0  0 | | 4 |   | 46 |
;; | 0  6  7  8  0 | | 3 |   | 61 |
;; | 0  0  9 10 11 | | 2 |   | 58 |
;; \ 0  0  0 12 13 / \ 1 /   \ 37 /
(test*
 "test-csr-mv"
 #(13 46 61 58 37)
 (let ((A (make-matrix
           5 5
           (make-csr
            #(1 2 3 4 5 6 7 8 9 10 11 12 13)
            #(0 2 5 8 11 13)
            #(0 1 0 1 2 1 2 3 2 3 4 3 4))))
       (x #(5 4 3 2 1)))
   (mv A x)))

(let ((A (make-matrix
          5 5
          (make-csr
           #(1 2 2 4 5 5 7 8 8 10 11 11 13)
           #(0 2 5 8 11 13)
           #(0 1 0 1 2 1 2 3 2 3 4 3 4))))
      (answer #(5 4 3 2 1))
      (b #(13 41 57 55 35))
      (check (lambda (expected result-of-thunk)
               (vector-every
                (lambda (x) (< (abs x) 1e-10))
                (vector-map - expected result-of-thunk)))))
  (test*
   "test-cg-solve"
   answer
   (cg-solve A b :eps 1e-13 :max-iter 10)
   check)
  (test*
   "test-pcg-solve (diagonal scaling)"
   answer
   (pcg-solve A b :eps 1e-13 :max-iter 10 :precond! (make-diag-precond A))
   check))

(test*
 "test-func->fel-3x3:x"
 #(0. 0.5 1. 0. 0.5 1. 0. 0.5 1.)
 (func->fel (mesh2d-unit-square 3 3) (lambda (x y) x)))

(test*
 "test-func->fel-3x3:y"
 #(0. 0. 0. 0.5 0.5 0.5 1. 1. 1.)
 (func->fel (mesh2d-unit-square 3 3) (lambda (x y) y)))

(let ((Th (mesh2d-unit-square 5 5))
      (x (lambda (x y) x))
      (y (lambda (x y) y))
      (x^2+y^2 (lambda (x y) (+ (* x x) (* y y)))))
  (test*
   "test-func->fel-5x5:(x^2+y^2)"
   (vector-map x^2+y^2 (func->fel Th x) (func->fel Th y))
   (func->fel Th x^2+y^2)))

(test*
 "test-eval-at"
 (inexact 1/16)
 (let ((Th (mesh2d-unit-square 5 5)))
   (eval-at Th #(1/12 1/6)
            (func->fel Th (lambda (x y) (+ (* x x) (* y y)))))))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
