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
   mesh2d-adherent?
   mesh2d-nodes
   mesh2d-nodes-length
   mesh2d-nodes-ref
   mesh2d-unit-square
   mesh2d-ith-triangle
   mesh2d-triangles
   mesh2d-triangles-for-each
   mesh2d-triangles-length
   mesh2d-trinix-for-each
   mesh2d-trinix-ref
   mesh2d-trinix->trig
   )
  )

(select-module cafemielk.mesh)

(dynamic-load "cafemielk__mesh")


(define-class <mesh2d> ()
  ((nodes :init-keyword :nodes)
   (triangles :init-keyword :triangles)))

(define (make-mesh2d nodes triangles)
  (make <mesh2d> :nodes nodes :triangles triangles))

(define (mesh2d-nodes mesh)
  (slot-ref mesh 'nodes))

(define (mesh2d-triangles mesh)
  (slot-ref mesh 'triangles))

(define (mesh2d-nodes-length mesh)
  (vview-length (mesh2d-nodes mesh) 0))

(define (mesh2d-triangles-length mesh)
  (vview-length (mesh2d-triangles mesh) 0))

(define (mesh2d-nodes-ref mesh i)
  (vview-cut (mesh2d-nodes mesh) (vector i)))

(define (mesh2d-trinix->trig mesh trinix)
  (define (node i)
    (vview->vector (mesh2d-nodes-ref mesh (vector-ref trinix i))))
  (define (x_ i) (vector-ref (node i) 0))
  (define (y_ i) (vector-ref (node i) 1))
  (vector (x_ 0) (x_ 1) (x_ 2)
          (y_ 0) (y_ 1) (y_ 2)))

(define (mesh2d-trinix-ref mesh i)
  (vview-cut (mesh2d-triangles mesh) (vector i)))

(define (mesh2d-trinix-for-each proc mesh)
  (define N (mesh2d-triangles-length mesh))
  (let loop ((t 0))
    (cond
     ((= t N) #f)
     (else (proc (vview->vector (mesh2d-trinix-ref mesh t)))
           (loop (+ t 1))))))

(define (mesh2d-ith-triangle mesh t)
  (mesh2d-trinix->trig
   mesh
   (vview->vector (mesh2d-trinix-ref mesh t))))

(define (mesh2d-triangles-for-each proc mesh)
  (mesh2d-trinix-for-each
   (lambda (trinix) (proc (mesh2d-trinix->trig mesh trinix)))
   mesh))

;; Geometric predicates

(define-inline (mesh2d-adherent? p trig)
  (define s (vector-ref p 0))
  (define t (vector-ref p 1))
  (define (x_ i) (vector-ref trig i))
  (define (y_ i) (vector-ref trig (+ i 3)))
  (define (dx_ i j) (- (x_ j) (x_ i)))
  (define (dy_ i j) (- (y_ j) (y_ i)))
  (every
   (lambda (a) (not (negative? a)))
   (list (- (* (dx_ 0 1) (- t (y_ 1))) (* (dy_ 0 1) (- s (x_ 1))))
         (- (* (dx_ 1 2) (- t (y_ 2))) (* (dy_ 1 2) (- s (x_ 2))))
         (- (* (dx_ 2 0) (- t (y_ 0))) (* (dy_ 2 0) (- s (x_ 0)))))))


;; Mesh utility

(define (mesh2d-unit-square nx ny)
  (define ns (%square-point-vec (vector-linspace 0. 1. nx)
                                (vector-linspace 0. 1. ny)))
  (define ts (%square-triangle-vec nx ny))
  (make-mesh2d
   (make-vview ns 0 (vector (* nx ny) 2))
   (make-vview ts 0 (vector (* 2 (- nx 1) (- ny 1)) 3))))
