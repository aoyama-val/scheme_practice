(define (tak x y z)
  (display (list x y z))
  (display "\n")
  (cond ((<= x y)
         y)
        (else
          (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y)))))

(display (tak 4 1 0))
