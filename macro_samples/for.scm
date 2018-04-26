(define-syntax for
  (syntax-rules ()
                ((_ (i from to step) b1 ...)
                 (let loop ((i from))
                   (if (< i to)
                     (begin
                       b1
                       ...
                       (loop (+ i step))))))
                ((_ (i from to) b1 ...)
                 (for (i from to 1) b1 ...))))


(for (x 3 6) (print "hoge") (print x))
(for (x 3 6 2) (print "hoge") (print x))
