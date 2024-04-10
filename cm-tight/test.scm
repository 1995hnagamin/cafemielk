;;;
;;; Test cafemielk
;;;

(use gauche.test)

(test-start "cafemielk")
(use cafemielk)
(test-module 'cafemielk)

;; The following is a dummy test code.
;; Replace it for your tests.
(test* "test-cafemielk" "cafemielk is working"
       (test-cafemielk))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
