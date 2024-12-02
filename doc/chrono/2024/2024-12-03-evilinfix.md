2024-12-03 邪悪な中置記法の案
==========

有限要素行列を構成するときに， `(dNi/dxj 3 0)` のような式を沢山書くことになるが，
もう少し直截的な書き方ができると嬉しい．

```cl
(defun parse-evil-infix-subscript (stream &rest rest)
  (declare (ignore rest))
  (read-char stream) ; read #\(
  (let ((func nil)
    (args nil))
    (loop for peeked = (peek-char t stream) do
      (case peeked
        (#\(
         (read-char stream) ; read #\(
         (setq args (cons (read stream) args))
         (read-char stream) ; read #\)
         (setq func (cons #\] (cons #\[ func))))
        (#\)
         (read-char stream); read #\)
         (return
           `(,(intern (string-upcase
               (coerce (reverse func) 'string)))
          ,@(reverse args)))
         (return nil))
        (otherwise
         (setq func (cons (read-char stream) func)))))))

(set-dispatch-macro-character
 #\# #\i
 #'parse-evil-infix-subscript)


(defun dN[]/dx[] (i j)
  (print i)
  (print j))
```

こうすると， `#i(dN(3)/dx(0))` のように書ける．
しかし， `[]` 内に複雑な式を書こうとすると ``#i(dN((+ i 1))/dx((mod (+ j 1) 3)))`` のようになって見苦しい．
もっと良い書き方はあるか．

--------
Author: hnagamin.
Permanent ID of this document: `a27ec286cc65735c18e442d02d3fbdeb5e2259baw`.
Date: 2024-12-03.
