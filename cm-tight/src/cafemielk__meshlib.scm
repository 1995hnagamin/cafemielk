;;;
;;; cafemielk__meshlib.scm
;;;

;; This line ensures defined bindings are inserted into cafemielk.mesh.
(in-module cafemielk.mesh)

(inline-stub
 (.include "cafemielk__mesh.h")

 ;; The 'define-cproc' forms exposes C functions to Scheme world.

 (define-cproc %square-point-vec (xvec yvec)
   (return (square_point_vec xvec yvec)))

 (define-cproc %square-triangle-vec (nx::<int> ny::<int>)
   (return (square_triangle_vec nx ny)))
 )


;; Local variables:
;; mode: scheme
;; end:
