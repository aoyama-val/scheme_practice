(define (split-find x lis)
  (let loop ((lis lis) (lis1 '()))
    (cond
      ((null? lis) (values (reverse lis1) '()))
      ((equal? (car lis) x) (values (reverse lis1) lis))
      (else (loop (cdr lis) (cons (car lis) lis1))))))

(print (call-with-values (lambda () (split-find 'c '(a b c d e f))) list))

; (a b)
; (c d e f)
