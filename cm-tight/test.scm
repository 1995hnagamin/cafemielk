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

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
