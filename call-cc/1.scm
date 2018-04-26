; call/ccのサンプル


(define exit-cont '())
(call/cc (lambda (cont)
           (set! exit-cont (lambda ()
                             (print "yes")
                             (cont)
                             (print "never reach here")))))

(define cont2 '())
(define (sum)
  (let loop ((i 1))
    (print i)
    (cond ((= i 10)
           (exit-cont))
          ((= i 4)
           (call/cc (lambda (cont)
                      (set! cont2 cont)))
           (loop  (+ i 1)))
          (else
            (loop  (+ i 1))))))

(print "start sum")
(sum)
(print "end sum")

(print "start cont2")
(cont2)
(print "end cont2")
