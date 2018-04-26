(define (decode lis)
  (apply append (map (lambda (pair) (repeat (car pair) (cdr pair))) lis)))

(define (repeat x n)
  (let loop ((i 0) (result '()))
    (cond
      ((= i n) result)
      (else (loop (+ i 1) (cons x result))))))

(print (repeat 'a 0))
(print (repeat 'a 1))
(print (repeat 'a 2))
(print (repeat 'a 3))

(print (decode '((a . 3) (b . 2) (c . 1) (d . 5) (e . 1))))
; (a a a b b c d d d d d e)
