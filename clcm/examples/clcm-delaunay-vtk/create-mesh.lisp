(defpackage :clcm-delaunay-vtk
  (:use :cl)
  (:local-nicknames (:cm :cafemielk))
  (:export
   :run-analysis))

(in-package :clcm-delaunay-vtk)

(defun run-analysis ()
  (format t "Cafemielk version: ~a~%~%" (cm:cafemielk-version))
  (format t "Bye.~%"))
