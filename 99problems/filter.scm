(define (make-index-pair lis)
  (let loop ((i 0) (lis lis) (result '()))
    (cond
      ((null? lis) (reverse result))
      (else (loop (+ i 1) (cdr lis) (cons (list (car lis) i) result))))))

(define (filter-with-index f lis)
  (map car (filter (lambda (x) (apply f x)) (make-index-pair lis))))

(define test1 '(a b c d e f g))

(print (make-index-pair '(a b c d e f)))
(print (filter-with-index (lambda (x i) (odd? i)) test1))

(define (map-with-index f lis)
  (map (lambda (x) (apply f x)) (make-index-pair lis)))

(print (map-with-index (lambda (x i) (+ x i)) '(11 22 33)))
