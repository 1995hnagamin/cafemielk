;;;
;;; cafemielk.mesh
;;; Mesh library
;;;

(define-module cafemielk.mesh
  (use cafemielk.util)
  (use cafemielk.vview)
  (use srfi-133)
  (use srfi-210)
  (export
   <mesh2d>
   make-mesh2d
   mesh2d-trigs-elt
   mesh2d-trigs-for-each
   mesh2d-unit-square
   mesh2d-vertices
   mesh2d-vertices-for-each-with-index
   mesh2d-vertices-length
   mesh2d-vertices-ref
   mesh2d-vise-for-each
   mesh2d-vise-ref
   mesh2d-vise->trig
   mesh2d-vises
   mesh2d-vises-length
   trig2d-adherent?
   )
  )

(select-module cafemielk.mesh)

(dynamic-load "cafemielk__mesh")


(define-class <mesh2d> ()
  ((vertices :init-keyword :vertices) ; vview of x/y-coordinates of vertices
   (vises :init-keyword :vises))) ; vview of vertex indices of triangles

(define (make-mesh2d vtxs vises)
  (make <mesh2d> :vertices vtxs :vises vises))

(define (mesh2d-vertices mesh)
  (slot-ref mesh 'vertices))

(define (mesh2d-vises mesh)
  (slot-ref mesh 'vises))

(define (mesh2d-vertices-length mesh)
  (vview-length (mesh2d-vertices mesh) 0))

 (define (mesh2d-vises-length mesh)
  (vview-length (mesh2d-vises mesh) 0))

(define (mesh2d-vertices-ref mesh i)
  (vview-cut (mesh2d-vertices mesh) (vector i)))

(define (mesh2d-vertices-for-each-with-index proc mesh)
  (define N (mesh2d-vertices-length mesh))
  (define vtxs (mesh2d-vertices mesh))
  (let loop ((i 0))
    (cond
     ((= i N) #f)
     (else
      (proc i (vview-ref vtxs (vector i 0)) (vview-ref vtxs (vector i 1)))
      (loop (+ i 1))))))

(define (mesh2d-vise->trig mesh vise)
  (define (node i)
    (vview->vector (mesh2d-vertices-ref mesh (vector-ref vise i))))
  (define (x_ i) (vector-ref (node i) 0))
  (define (y_ i) (vector-ref (node i) 1))
  (vector (x_ 0) (x_ 1) (x_ 2)
          (y_ 0) (y_ 1) (y_ 2)))

(define (mesh2d-vise-ref mesh i)
  (vview-cut (mesh2d-vises mesh) (vector i)))

(define (mesh2d-vise-for-each proc mesh)
  (define N (mesh2d-vises-length mesh))
  (let loop ((t 0))
    (cond
     ((= t N) #f)
     (else (proc (vview->vector (mesh2d-vise-ref mesh t)))
           (loop (+ t 1))))))

(define (mesh2d-trigs-elt mesh t)
  (mesh2d-vise->trig
   mesh
   (vview->vector (mesh2d-vise-ref mesh t))))

(define (mesh2d-trigs-for-each proc mesh)
  (mesh2d-vise-for-each
   (lambda (vise) (proc (mesh2d-vise->trig mesh vise)))
   mesh))

;; Geometric predicates

(define-inline (trig2d-adherent? trig p)
  (every-substit-A3
   (i i+1 i+2)
   (not (negative?
         (- (* (- (trig2d-xref trig i+1) (trig2d-xref trig i))
               (- (vector-ref p 1)       (trig2d-yref trig i+1)))
            (* (- (trig2d-yref trig i+1) (trig2d-yref trig i))
               (- (vector-ref p 0)       (trig2d-xref trig i+1))))))))


;; Mesh utility

(define (mesh2d-unit-square nx ny)
  (define ns (%square-point-vec (vector-linspace 0. 1. nx)
                                (vector-linspace 0. 1. ny)))
  (define ts (%square-triangle-vec nx ny))
  (make-mesh2d
   (make-vview ns 0 (vector (* nx ny) 2))
   (make-vview ts 0 (vector (* 2 (- nx 1) (- ny 1)) 3))))
