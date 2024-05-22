;;;
;;; cafemielk
;;;

(define-module cafemielk
  (extend
   cafemielk.util
   cafemielk.linalg
   cafemielk.vview)
  (export
   <mesh2d>
   eval-at-triangle
   func->fel
   make-mesh2d
   mesh2d-nodes
   mesh2d-nodes-length
   mesh2d-nodes-ref
   mesh2d-triangles
   mesh2d-triangles-ref
   square
   )
  )
(select-module cafemielk)

;; Loads extension
(dynamic-load "cafemielk")

;;
;; Put your Scheme definitions here
;;

;; cafemielk.mesh

(use cafemielk.util)
(use cafemielk.vview)

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

(define (mesh2d-nodes-ref mesh i)
  (vview-cut (mesh2d-nodes mesh) (vector i)))

(define (mesh2d-triangles-ref mesh i)
  (vview-cut (mesh2d-triangles mesh) (vector i)))

(define (square nx ny)
  (define ns (square-point-vec (vector-linspace 0. 1. nx)
                               (vector-linspace 0. 1. ny)))
  (define ts (square-triangle-vec nx ny))
  (make-mesh2d
   (make-vview ns 0 (vector (* nx ny) 2))
   (make-vview ts 0 (vector (* 2 (- nx 1) (- ny 1)) 3))))


;; cafemielk.fel

(define (func->fel Th func)
  (vector-tabulate
   (mesh2d-nodes-length Th)
   (lambda (i)
     (let ((p (mesh2d-nodes-ref Th i)))
       (func (vview-ref p #(0))
             (vview-ref p #(1)))))))

;; p should be in tth triangle of Th
(define (eval-at-triangle p f mesh t)
  (define nodes
    (vector-map
     (lambda (i) (vview->vector (mesh2d-nodes-ref mesh i)))
     (vview->vector (mesh2d-triangles-ref mesh t))))
  (define xt
    (vector-tabulate 3 (lambda (i) (vector-ref (vector-ref nodes i) 0))))
  (define yt
    (vector-tabulate 3 (lambda (i) (vector-ref (vector-ref nodes i) 1))))
  (define a (cross3 xt yt))
  (define b (vector-tabulate
             3
             (lambda (i) (- (vector-ref yt (modulo (+ i 1) 3))
                            (vector-ref yt (modulo (+ i 2) 3))))))
  (define c (vector-tabulate
             3
             (lambda (i) (- (vector-ref xt (modulo (+ i 2) 3))
                            (vector-ref xt (modulo (+ i 1) 3))))))
  (define mass (vector-map
                (lambda (i) (vector-ref f i))
                (vview->vector (mesh2d-triangles-ref mesh t))))
  (/ (+ (dot mass a)
        (* (dot mass b) (vector-ref p 0))
        (* (dot mass c) (vector-ref  p 1)))
     (cross2 (vector-map - (vector-ref nodes 1) (vector-ref nodes 0))
             (vector-map - (vector-ref nodes 2) (vector-ref nodes 0)))))
