(define (my-map f lis)
  (cond ((null? lis) '())
  (else (cons (f (car lis)) (my-map f (cdr lis))))))

(display (my-map (lambda (x) (* x x)) '(1 2 3))) (newline)
