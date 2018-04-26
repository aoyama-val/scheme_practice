; ループバージョン
(define (count-leaf tree)
  (if (pair? tree)
    (let loop ((tree tree))
      (cond
        ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaf (car tree)) (loop (cdr tree))))))
    1))

; 再帰のみバージョン
(define (count-leaf tree)
  (cond
    ((null? tree) 0)
    ((pair? tree) (+ (count-leaf (car tree)) (count-leaf (cdr tree))))
    (else 1)))

(print (count-leaf 'a))
(print (count-leaf '(a)))
(print (count-leaf '(a . b)))
(print (count-leaf '(a b . c)))
(print (count-leaf '(a b (c d))))

(print (count-leaf '(a (b (c (d . e) f) g) h)))
; 8
