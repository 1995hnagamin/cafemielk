;;;
;;; cafemielk
;;;

(define-module cafemielk
  (export test-cafemielk ;; dummy
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

(define (square nx ny)
  (define ns (square-point-vec (vector-linspace 0. 1. nx)
	         	       (vector-linspace 0. 1. ny)))
  (define ts (square-triangle-vec nx ny))
  (make-mesh2d
   (make-vview ns 0 (vector (* nx ny) 2))
   (make-vview ts 0 (vector (* 2 (- nx 1) (- ny 1)) 3))))
