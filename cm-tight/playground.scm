(use cafemielk)

(print "Loading playground file...")

(define-class <cm-csr> ()
    ((nrows :init-keyword :nrows)
     (ncols :init-keyword :ncols)
     (data :init-keyword :data)
     (indices :init-keyword :indices)
     (indptr :init-keyword :indptr)))
