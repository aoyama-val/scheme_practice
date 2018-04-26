(define (remove-last lis)
  (let loop ((lis lis) (ret '()))
    (cond
      ((null? lis) (error "last: null lis"))
      ((null? (cdr lis))
       (values
         (reverse ret)
         (car lis)))
      (else
        (loop (cdr lis) (cons (car lis) ret))))))

;; 右論理シフト
(define (uint-srl uint)
    (receive (ret last) (remove-last uint)
             (values
               (cons #f ret)
               last)))

;; 左論理シフト
(define (uint-sll uint)
  (values
    (append (cdr uint) (list #f))
    (car uint)
    ))


(receive (ret lsb) (uint-srl '(#t #f #t #f))
         (print ret)
         (print lsb)
         )
; (#f #t #f #t)
; #f
(receive (ret lsb) (uint-srl '(#f #t #f #t))
         (print ret)
         (print lsb)
         )
; (#f #f #t #f)
; #t
(receive (ret msb) (uint-sll '(#t #f #t #f))
         (print ret)
         (print msb))

; (#f #t #f #f)
; #t
(receive (ret msb) (uint-sll '(#f #t #f #f))
         (print ret)
         (print msb))
; (#t #f #f #f)
; #f
