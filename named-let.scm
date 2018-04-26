(define (dp lis1 lis2)
  (cond ((null? lis1) '())
        (else (append
                (map (lambda (x) (list (car lis1) x )) lis2)
                (dp (cdr lis1) lis2)))))

(display (dp (dp '(1 2 3) '("hoge" "moge")) '(#t #f)))
