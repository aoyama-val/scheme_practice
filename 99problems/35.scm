; バージョン1
(define (partition lis)
  (let loop ((i 0) (lis lis) (odds '()) (evens '()))
    (cond
      ((null? lis) (values (reverse odds) (reverse evens)))
      ((= 0 (mod i 2)) (loop (+ i 1) (cdr lis) (cons (car lis) odds) evens))
      (else (loop (+ i 1) (cdr lis) odds (cons (car lis) evens))))))

; 作ったけど結局使わなかったもの
(define (map-with-index f lis)
  (let loop ((i 0) (lis lis) (result '()))
    (cond
      ((null? lis) (reverse result))
      (else
        (loop (+ i 1) (cdr lis) (cons (f (car lis) i) result))))))

; バージョン2
(define (filter-with-index f lis)
  (let loop ((i 0) (lis lis) (result '()))
    (cond
      ((null? lis) (reverse result))
      (else
        (loop (+ i 1) (cdr lis)
              (if (f (car lis) i)
                (cons (car lis) result)
                result))))))

; バージョン2
(define (partition lis)
  (values
    (filter-with-index (lambda (x i) (even? i)) lis)
    (filter-with-index (lambda (x i) (odd? i)) lis)))

;(print (filter-with-index (lambda (x i) (= (mod i 2) 0)) '(a b c d e f g)))



(print (call-with-values (lambda () (partition '(a b c d e f g))) list))

;(a c e g)
;(b d f)
