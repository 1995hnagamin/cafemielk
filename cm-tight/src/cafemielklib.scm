;;;
;;; cafemielklib.stub
;;;

(inline-stub
 (declcode
  (.include "cafemielk.h"))

 ;; The following entry is a dummy one.
 ;; Replace it for your definitions.

 (define-cproc square-point-vec (xvec yvec)
   (return (square_point_vec xvec yvec)))

 (define-cproc square-triangle-vec (nx::<int> ny::<int>)
   (return (square_triangle_vec nx ny))))


;; Local variables:
;; mode: scheme
;; end:
