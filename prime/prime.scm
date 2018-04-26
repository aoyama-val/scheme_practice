; a mod b == 0 ?
(define (dividable? a b)
  (if (= 0 (mod a b))
    #t
    #f))

; n0が素数かどうか
(define (prime? n0)
  (define (prime-aux n x)
    (cond ((= n 1) #f)
          ((>= x n) #t)
          ((dividable? n x) #f)
          (else (prime-aux n (+ x 1)))))
  (prime-aux n0 2))

;(display (filter prime? '(1 2 3 4 5 6 7 8 9 10 11)))
;(newline)

; nがlisのいずれかで割り切れるか
(define (dividable-any? n lis)
  (any (lambda (x) (dividable? n x)) lis))

; ふるいを使って素数のリストを求める
(define (sieve n max accum)
  (cond ((> n max) accum)
        (else (sieve (+ n 1) max (if (dividable-any? n accum) accum (cons n accum))))))

(display (sieve 2 100 '()))
(newline)
