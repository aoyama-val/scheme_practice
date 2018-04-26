(define (int->uint n m)
  (cond
    ((= m 0) '())
    (else
      (append (int->uint (quotient n 2) (- m 1))
               (list (if (= (mod n 2) 0) #f #t))))))

(define (uint->int x)
  (let loop ((result 0) (rest x))
    (cond
      ((null? rest) result)
      (else
        (loop (+ (* 2 result) (if (car rest) 1 0)) (cdr rest))))))
(define (xor p q)
  (or
    (and (not p) q)
    (and p (not q))))

(define (half-adder p q)
  (values
    (xor p q)
    (and p q)))

(define (full-adder p q r)
  (receive (s c) (half-adder p q)
           (values
             (xor s r)
             (or
               (and p q)
               (and r (xor p q))))))

(define (uint-add i1 i2)
  (cond
    ((null? i1) (values i2 #f))
    ((null? i2) (values i1 #f))
    (else
      (receive (next-sum next-carry) (uint-add (cdr i1) (cdr i2))
               (receive (s c) (full-adder (car i1) (car i2) next-carry)
                        (values
                          (cons s next-sum)
                          c))))))
;; 左論理シフト
(define (uint-sll uint)
  (values
    (append (cdr uint) (list #f))
    (car uint)
    ))

(define (uint-mul i1 i2)
  (let loop (
             (ret (make-list (length i1) #f))
             (m (reverse i2))
             (shifted-i1 i1))
    (cond
      ((null? m) ret)
      ((car m)
       (loop
         (uint-add ret shifted-i1)
         (cdr m)
         #?=(uint-sll shifted-i1)))
      (else
        (loop
          ret
          (cdr m)
          #?=(uint-sll shifted-i1))))))


(define (print-mul-table)
  (let loop ((i 0))
    (cond
      ((= i 16) #f)
      (else
        (let loop2 ((j 0))
          (cond
            ((= j 16) #f)
            ((not (= (uint->int (uint-mul (int->uint i 4) (int->uint j 4))) (mod (* i j) 16)))
                  (print "Test failure not equal"))
            (else
              (print (format "~D * ~D = ~D" i j (uint->int (uint-mul (int->uint i 4) (int->uint j 4)))))
              (loop2 (+ j 1)))))
        (loop (+ i 1))))))


(print-mul-table)
;(print (uint->int (uint-mul #?=(int->uint 2 4) #?=(int->uint 4 4))))

;(print (uint->int (uint-mul '(#f #f #t #t) '(#f #f #t #t))))
; (#t #f #f #t)
;(print (uint-mul '(#t #f #f #f) '(#f #f #t #f)))
; (#f #f #f #f)
