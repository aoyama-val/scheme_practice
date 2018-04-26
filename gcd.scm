; 最大公約数

(define (gcd a b)
  (cond ((< a b) (gcd b a))
        ((= 0 (mod a b)) b)
        (else (gcd b (mod a b)))))

(display (gcd 12 3)) (newline)
(display (gcd 4 12)) (newline)
(display (gcd 8 12)) (newline)
(display (gcd 100 40)) (newline)

(define (mymod a b)
  (cond ((< a b) a)
        (else (mymod (- a b) b))))


(define a 1000000000)
(define b 7)
(time (display (mymod a b)))
(newline)
(time (display (mod a b)))
(newline)
