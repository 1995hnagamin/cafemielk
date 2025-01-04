(defpackage :cafemielk-tests/all
  (:use :cl :cafemielk)
  (:export :run-tests))

(in-package :cafemielk-tests/all)

(defun run-tests ()
  (format t "Running Cafemielk tests (0/0)...~%")
  t)
