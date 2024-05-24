;;;
;;; cafemielk.fel
;;; Finite elements
;;;

(define-module cafemielk.fel
  (use cafemielk.mesh)
  (use cafemielk.util)
  (use cafemielk.vview)
  (export
   eval-at-triangle
   func->fel
   )
  )

(select-module cafemielk.fel)

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
