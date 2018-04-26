(define (serialize pair)
  (cond
    ((pair? pair)
     (append (list 0) (serialize (car pair)) (serialize (cdr pair))))
    (else
      (list 1 pair))))


(print (serialize '(a . b)))
; (0 1 a 1 b)
(print (serialize '((a . b) . c)))
; (0 0 1 a 1 b 1 c)
(print (serialize '((a . b) (c . d))))
; (0 0 1 a 1 b 0 0 1 c 1 d 1 ())
(print (serialize '(a (b (c . d) e) f)))
; (0 1 a 0 0 1 b 0 0 1 c 1 d 0 1 e 1 () 0 1 f 1 ())
