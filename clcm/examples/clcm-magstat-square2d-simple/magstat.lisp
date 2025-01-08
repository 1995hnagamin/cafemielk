(defpackage :clcm-magstat-square2d-simple
  (:use :cl)
  (:local-nicknames (:cm :cafemielk))
  (:export
   :run-analysis))

(in-package :clcm-magstat-square2d-simple)

(defun run-analysis ()
  (format t "Hello, World~%")
  (format t "Cafemielk version: ~a~%" (cm:cafemielk-version)))
