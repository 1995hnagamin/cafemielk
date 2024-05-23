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

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
