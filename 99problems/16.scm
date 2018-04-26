(define (iota start end)
  (cond ((> start end) '())
        (else (cons start (iota (+ 1 start) end)))))


(print (iota 1 5))
; (1 2 3 4 5)
