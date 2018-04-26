(define (comb-num n r)
  (/ (/ (factorial n) (factorial r)) (factorial (- n r))))

(define (factorial n)
  (if (= n 0)
    1
    (* n (factorial (- n 1)))))



(print (comb-num 5 3))
; 10
(print (comb-num 10 5))
; 252
