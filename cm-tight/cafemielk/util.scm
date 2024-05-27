;;;
;;; cafemielk.util
;;;

(define-module cafemielk.util
  (use srfi.133)
  (export
   linspace
   vector-linspace
   collect-replace
   collect-replace-A3
   cross2d
   cross3d
   dot
   every-replace-A3
   trig2d-area
   trig2d-prod
   trig2d-xref
   trig2d-yref
   vec3d-tab
   vector-unzip2
   )
  )

(select-module cafemielk.util)


;; analogue of fold-map
;;
;; `(,collect ,@(map (lambda ,param-list expr)
;;                   ,arg-list-list))
;;
(define-syntax collect-replace
  (syntax-rules ()
    ((_ collect ((param ...) (arg ...) ...) expr)
     (let-syntax
         ((component (syntax-rules ()
                       ((_ param ...) expr))))
       (collect (component arg ...) ...)))))

(define-syntax collect-replace-A3
  (syntax-rules ()
    ((_ collect (i j k) expr)
     (collect-replace collect
                      ((i j k)
                       (0 1 2) (1 2 0) (2 0 1))
                      expr))))

(define-syntax every-replace-A3
  (syntax-rules ()
    ((_ (i j k) expr)
     (collect-replace-A3 and (i j k) expr))))

(define-syntax vec3d-tab
  (syntax-rules ()
    ((_ (i j k) expr)
     (collect-replace-A3 vector (i j k) expr))))

(define (linspace min max size)
  (define step (/ (- max min) (- size 1)))
  (map (lambda (i) (+ min (* step i))) (iota size)))

(define (vector-linspace min max size)
  (define step (/ (- max min) (- size 1)))
  (vector-tabulate size (lambda (i) (+ min (* step i)))))

(define-inline (cross2d u v)
  (- (* (vector-ref u 0) (vector-ref v 1))
     (* (vector-ref u 1) (vector-ref v 0))))

(define-inline (cross3d u v)
  (define (u_ i) (vector-ref u i))
  (define (v_ i) (vector-ref v i))
  (vec3d-tab
   (i i+1 i+2)
   (- (* (u_ i+1) (v_ i+2)) (* (u_ i+2) (v_ i+1)))))

(define (dot u v)
  (vector-fold (lambda (acc ui vi) (+ acc (* ui vi))) 0 u v))

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

(define (vector-unzip2 vector-of-vectors)
  (define N (vector-length vector-of-vectors))
  (define vec1 (make-vector N))
  (define vec2 (make-vector N))
  (let loop ((i 0))
    (cond
     ((= i N)
      (values vec1 vec2))
     (else
      (vector-set! vec1 i (vector-ref (vector-ref vector-of-vectors i) 0))
      (vector-set! vec2 i (vector-ref (vector-ref vector-of-vectors i) 1))
      (loop (+ i 1))))))
