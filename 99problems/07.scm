(define (take xs n)
  (let loop ((i 0) (lis xs) (ret '()))
    (cond
      ((null? lis) (reverse ret))
      ((= i n) (reverse ret))
      (else (loop (+ i 1) (cdr lis) (cons (car lis) ret))))))

(define (drop lis n)
  (cond ((null? lis) '())
        ((= n 0) lis)
        (else (drop (cdr lis) (- n 1)))))

(define (subseq xs n m)
  (take (drop xs n) (- m n)))


(print (subseq '(a b c d e) 2 4))
;(c d)
(print (subseq '(a b c d e) 0 5))
;(a b c d e)
(print (subseq '(a b c d e) 0 0))
;()
