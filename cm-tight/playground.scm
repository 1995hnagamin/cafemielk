(use cafemielk)
(use gauche.time)

(print "Loading playground file...")

(define A (make <csr> :nrows 6 :ncols 6
                :vals #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
                :rowptr #(0 2 5 8 11 14 16)
                :colind #(0 1 0 1 2 1 2 3 2 3 4 3 4 5 4 5)))

(define x #(1 2 3 4 5 6))

