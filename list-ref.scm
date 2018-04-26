(define (list-ref lis n)
  (cond ((null? lis) #f)
        ((< n 0) (list-ref lis (+ (length lis) n)))
        ((= n 0) (car lis))
        (else (list-ref (cdr lis) (- n 1)))))

(let ((a '("hoge" "moge" "sage")))
  (display (list-ref a -3)))

