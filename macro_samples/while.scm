(define-syntax while
  (syntax-rules ()
    ((_ pred b1 ...)
     (let loop ()
       (if pred
         (begin
           b1
           ...
           (loop)))))))

(let ((i 0))
  (while (< i 4)
        (print i)
        (inc! i)))
