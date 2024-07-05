;;;
;;; cafemielk.util
;;;

(define-module cafemielk.util
  (use srfi.133)
  (export
   collect-substit
   collect-substit-A3
   cross2d
   cross3d
   dot
   every-substit-A3
   linspace
   vec3d-tab
   vector-linspace
   vector-unzip2
   )
  )

(select-module cafemielk.util)


;; analogue of fold-map
;;
;; `(,collect ,@(map (lambda ,param-list expr)
;;                   ,arg-list-list))
;;
(define-syntax collect-substit
  (syntax-rules ()
    ((_ collect ((param ...) (arg ...) ...) expr)
     (let-syntax
         ((component (syntax-rules ()
                       ((_ param ...) expr))))
       (collect (component arg ...) ...)))))

(define-syntax collect-substit-A3
  (syntax-rules ()
    ((_ collect (i j k) expr)
     (collect-substit collect
                      ((i j k)
                       (0 1 2) (1 2 0) (2 0 1))
                      expr))))

(define-syntax every-substit-A3
  (syntax-rules ()
    ((_ (i j k) expr)
     (collect-substit-A3 and (i j k) expr))))

(define-syntax vec3d-tab
  (syntax-rules ()
    ((_ (i j k) expr)
     (collect-substit-A3 vector (i j k) expr))))

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
