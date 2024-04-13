;;;
;;; cafemielk.linalg
;;; Linear algebra library for Cafemielk
;;;

(define-module cafemielk.linalg
  (export <crs>
	  crs-addmv!
	  crs-mv
	  )
  )

(select-module cafemielk.linalg)

(define-class <crs> ()
  ((nrows :init-keyword :nrows)
   (ncols :init-keyword :ncols)
   (vals :init-keyword :vals)
   (rowptr :init-keyword :rowptr)
   (colind :init-keyword :colind)))

(define (crs-addmv! y A x)
  ; y += A*x
  (define m (vector-length y))
  (define n (vector-length x))
  (do ((i 0 (+ i 1)))
      ((= i m) #f)
    (do ((j (vector-ref (slot-ref A 'rowptr) i) (+ j 1)))
	((= j (vector-ref (slot-ref A 'rowptr) (+ i 1))) #f)
      (vector-set!
       y i
       (+ (vector-ref y i)
	  (* (vector-ref (slot-ref A 'vals) j)
	     (vector-ref x (vector-ref (slot-ref A 'colind) j))))))))


(define (crs-mv A x)
  (define y (make-vector (slot-ref A 'nrows) 0))
  (crs-addmv! y A x)
  y)
