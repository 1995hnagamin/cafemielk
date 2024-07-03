;;;
;;; cafemielk.geom.trig2d
;;;

(define-module cafemielk.geom.trig2d
  (use cafemielk.util)
  (export
   trig2d-adherent?
   trig2d-area
   trig2d-prod
   trig2d-xref
   trig2d-yref
   )
  )

(select-module cafemielk.geom.trig2d)


(define-inline (trig2d-adherent? trig pt)
  (every-substit-A3
   (i i+1 i+2)
   (not (negative?
         (- (* (- (trig2d-xref trig i+1) (trig2d-xref trig i))
               (- (vector-ref pt 1)      (trig2d-yref trig i+1)))
            (* (- (trig2d-yref trig i+1) (trig2d-yref trig i))
               (- (vector-ref pt 0)      (trig2d-xref trig i+1))))))))

(define-inline (trig2d-xref trig i)
  (vector-ref trig i))

(define-inline (trig2d-yref trig i)
  (vector-ref trig (+ i 3)))

(define-inline (trig2d-area trig)
  (define-syntax dd
    (syntax-rules ()
      ((_ i j)
       (vector (- (trig2d-xref trig j) (trig2d-xref trig i))
               (- (trig2d-yref trig j) (trig2d-yref trig i))))))
  (* 1/2 (cross2d (dd 0 1) (dd 0 2))))

(define (trig2d-prod trig u v)
  (define (x_ i) (vector-ref trig i))
  (define (y_ i) (vector-ref trig (+ i 3)))
  (dot v
       (vector-map
        (lambda (w) (dot u w))
        (vector (cross3d (vector-tabulate 3 x_) (vector-tabulate 3 y_))
                (vec3d-tab (i j k) (- (y_ k) (y_ i)))
                (vec3d-tab (i j k) (- (x_ k) (x_ j)))))))
