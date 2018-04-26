(define (deserialize lis)
  (cond
    ((= 0 (car lis))
     (receive (left rest) (deserialize (cdr lis))
              (receive (right rest2)
                       (deserialize rest)
                       (values
                         (cons left right)
                         rest2))))
    (else
      (values (cadr lis) (cddr lis)))))

;(receive (decoded rest) (deserialize '(1 a 1 b))
         ;(print decoded)
         ;(print rest))

(print (deserialize '(0 1 a 1 b)))
; (a . b)
; ()
(print (deserialize '(0 0 1 a 1 b 1 c)))
; ((a . b) . c)
; ()
(print (deserialize '(0 0 1 a 1 b 0 0 1 c 1 d 1 ())))
; ((a . b) (c . d))
; ()
