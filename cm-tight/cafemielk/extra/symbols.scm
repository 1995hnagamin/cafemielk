;;
;; cafemielk.extra.symbols
;;

(define-module cafemielk.extra.symbols
  (export $+ $- $* $/)
  )

(select-module cafemielk.extra.symbols)

(define ($+ . args)
  (cond
   ((= (length args) 1)
    (cadr args))
   ((= (length args) 2)
    `(+ ,(car args) ,(cadr args)))
   (else (error "Invalid number of arguments: $+"))))

(define ($- . args)
  (cond
   ((= (length args) 1)
    `(negate ,(car args)))
   ((= (length args) 2)
    `(- ,(car args) ,(cadr args)))
   (else (error "Invalid number of arguments: $-"))))

(define ($* a b) `(* ,a ,b))

(define ($/ a b) `(/ ,a ,b))

