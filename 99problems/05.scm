(define (take xs n)
  (let loop ((i 0) (lis xs) (ret '()))
    (cond
      ((null? lis) (reverse ret))
      ((= i n) (reverse ret))
      (else (loop (+ i 1) (cdr lis) (cons (car lis) ret))))))

(print (take '(a b c d e) 3))
(print (take '(a b c d e) 0))
(print (take '(a b c d e) 6))

