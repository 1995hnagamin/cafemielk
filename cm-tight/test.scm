;;;
;;; Test cafemielk
;;;

(use gauche.test)

(test-start "cafemielk")
(use cafemielk)
(test-module 'cafemielk)

(test*
 "test-cafemielk"
 #(b c d)
 (vview->vector (make-vview #(a b c d) 1 #(3))))

(test*
 "test-mesh2d-square"
 121
 (mesh2d-nodes-length (mesh2d-square 11 11)))

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

(test*
 "test-func->fel-3x3:x"
 #(0. 0.5 1. 0. 0.5 1. 0. 0.5 1.)
 (func->fel (mesh2d-square 3 3) (lambda (x y) x)))

(test*
 "test-func->fel-3x3:y"
 #(0. 0. 0. 0.5 0.5 0.5 1. 1. 1.)
 (func->fel (mesh2d-square 3 3) (lambda (x y) y)))

(let ((Th (mesh2d-square 5 5))
      (x (lambda (x y) x))
      (y (lambda (x y) y))
      (x^2+y^2 (lambda (x y) (+ (* x x) (* y y)))))
  (test*
   "test-func->fel-5x5:(x^2+y^2)"
   (vector-map x^2+y^2 (func->fel Th x) (func->fel Th y))
   (func->fel Th x^2+y^2)))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
