; mod（剰余）の再定義
(define (mymod a b)
  (if (< a b) a
    (mymod (- a b) b)))

(let ((a 25) (b 7))
  (display (mymod a b))
  (newline)
  (display (mymod 100 b))
  (newline))
