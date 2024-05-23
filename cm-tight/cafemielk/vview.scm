;;;
;;; cafemielk.vview
;;;

(define-module cafemielk.vview
  (use scheme.vector)
  (use gauche.sequence)
  (export
   <vview>
   make-vview
   vview-cut
   vview-length
   vview-rank
   vview-ref
   vview-shape
   vview-size
   vview-stratify
   vview->vector
   )
  )

(select-module cafemielk.vview)

(define-class <vview> ()
  ((vec :init-keyword :vec)
   (start :init-keyword :start)
   (shape :init-keyword :shape)
   (steps :init-keyword :steps)))

(define (make-vview vec start shape)
  (define dim (vector-length shape))
  (define steps (make-vector dim))
  (do ((i (- dim 1) (- i 1))
       (acc 1 (* acc (vector-ref shape i))))
      ((< i 0) #f)
    (vector-set! steps i acc))
  (make <vview> :vec vec :start start :shape shape :steps steps))

(define (vview-shape vv)
  (slot-ref vv 'shape))

(define (vview-start vv)
  (slot-ref vv 'start))

(define (vview-steps vv)
  (slot-ref vv 'steps))

(define (vview-data vv)
  (slot-ref vv 'vec))

(define (vview-size vv)
  (vector-fold * 1 (vview-shape vv)))

(define (vview-length vv r)
  (vector-ref (vview-shape vv) r))

(define (vview-rank vv)
  (vector-length (vview-shape vv)))

(define (vview->vector vv)
  (define start (vview-start vv))
  (define end (+ start (vview-size vv)))
  (vector-copy (vview-data vv) start end))

(define (ij->idx start steps ij)
  (vector-fold
   (lambda (seed a b) (+ seed (* a b)))
   start steps ij))

(define (vview-ref vv ij)
  (if (vector-every (lambda (i len) (<= 0 i (- len 1))) ij (vview-shape vv))
      (ref (vview-data vv)
           (ij->idx (vview-start vv) (vview-steps vv) ij))
      (error "vview-ref index out of range:" ij)))

(define (vview-cut vv i~)
  (if (vector-every (lambda (i len) (<= 0 i (- len 1))) i~ (vview-shape vv))
      (make-vview (vview-data vv)
                  (ij->idx (vview-start vv) (vview-steps vv) i~)
                  (vector-copy (vview-shape vv) (vector-length i~)))
      (error "vview-ref index out of range:" i~)))

(define (vview-stratify vv)
  (if (= (vview-rank vv) 1)
      (vview->vector vv)
      (vector-map (lambda (i)
                    (vview-stratify (vview-cut vv (make-vector 1 i))))
                  (vector-tabulate (vview-length vv 0) (lambda (i) i)))))
