(define (uint-inc i1)
  (uint-add i1 (append (make-list (- (length i1) 1) #f) (list #t))))
(define (uint-not i1)
  (map not i1))

(define (uint-neg x)
  (uint-inc (uint-not x)))
(define (uint-sub x y)
  (if (not (= (length x) (length y)))
    (error "uint-sub length not equal")
    (receive (s c) (uint-add x (uint-neg y))
           (values
             s
             (not c)))))
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

(define (uint-equal? i1 i2)
  (cond
    ((and (null? i1) (null? i2)) #t)
    ((null? i1) #f)
    ((null? i2) #f)
    ((eqv? (car i1) (car i2)) (uint-equal? (cdr i1) (cdr i2)))
    (else
      #f)))

(define (uint-zero? i1)
  (cond
    ((null? i1) #t)
    ((car i1) #f)
    (else
      (uint-zero? (cdr i1)))))

(define (uint-greater? i1 i2)
  (cond
    ((and (null? i1) (null? i2)) #f)
    ((null? i1) #f)
    ((null? i2) #t)
    ((and (car i1) (car i2)) (uint-greater? (cdr i1) (cdr i2)))
    ((and (car i1) (not (car i2))) #t)
    ((and (not (car i1)) (car i2)) #f)
    (else
      (uint-greater? (cdr i1) (cdr i2)))))

(define (uint-less? i1 i2)
  (cond
    ((uint-equal? i1 i2) #f)
    ((uint-greater? i1 i2) #f)
    (else
      #t)))

;; 左論理シフト
(define (uint-sll uint)
  (values
    (append (cdr uint) (list #f))
    (car uint)
    ))


(define (make-zero n)
  (make-list n #f))

(define (test-div a b expected)
  (let
    ((actual
       (uint->int
        (uint-div
          (int->uint a 4)
          (int->uint b 4)))))
    (if (= actual expected)
      (print "OK")
      (print "NG " a " / " b " = " expected " actual = " actual))))

(define (uint-div-aux i1 i2)
  (let loop ((i 0) (sum i2))
    (cond
      ((uint-greater? sum i1) (int->uint i (length i1)))
      (else
        (loop (+ i 1) (uint-add sum i2))))))

(define (uint-div-aux2 i1 i2)
  (cond
    ((uint-zero? i2)
     (values #f i1))
    ((uint-greater? i1 i2)
     (values #t (uint-sub i1 i2)))
    ((uint-equal? i1 i2)
     (values #t (uint-sub i1 i2)))
    (else
      (values #f i1))))

(define (uint-sll-n uint n)
  (cond
    ((= n 0) uint)
    (else
      (uint-sll-n (uint-sll uint) (- n 1)))))

; バージョン１　こちらの方を先に書いたが、こっちの方がきれいか
(define (uint-div i1 i2)
  (cond
    ((uint-greater? i1 i2)  ; i1 > i2 なら
     (if (car i2) ; i2の最上位ビットが立ってる時はi2をそれ以上シフトできないので、商1、引き算で余りを出す
       (values 
         '(#f #f #f #t)
        (uint-sub i1 i2))
       (receive (q r) (uint-div i1 (uint-sll i2)) ; i2を2倍した場合の商と余りを求める
                (if (uint-greater? r i2)
                  (values (uint-add (uint-sll q) '(#f #f #f #t))
                          (uint-sub r i2))
                  (receive (q2 r2) (uint-div r i2)
                           (values (uint-add (uint-sll q) q2) r2))))))
    ((uint-equal? i1 i2)  ; i1 = i2 なら (1,0)を返す
     (values '(#f #f #f #t) '(#f #f #f #f)))
    (else ; i1 < i2 なら (0,i1)を返す
      (values '(#f #f #f #f) i1))))

; バージョン2
(define (uint-div i1 i2)
  (cond
    ((car i2) ; i2の最上位ビットが立ってる時はi2をそれ以上シフトできないので、商1、引き算で余りを出す
     (if (uint-less? i1 i2)
       (values
         '(#f #f #f #f)
         i1)
       (values
         '(#f #f #f #t)
         (uint-sub i1 i2))))
    ((or (uint-equal? i1 (uint-sll i2)) (uint-greater? i1 (uint-sll i2)))  ; i1 > i2 * 2 なら
     (receive (q r) (uint-div i1 (uint-sll i2))
              (receive (q2 r2) (uint-div r i2)
                       (values
                         (uint-add (uint-sll q) q2)
                         r2))))
    ((uint-greater? i1 i2)  ; i1 > i2 なら
     (values
       '(#f #f #f #t)
       (uint-sub i1 i2)))
    ((uint-equal? i1 i2)  ; i1 = i2 なら (1,0)を返す
     (values '(#f #f #f #t) '(#f #f #f #f)))
    (else ; i1 < i2 なら (0,i1)を返す
      (values '(#f #f #f #f) i1))))

(define (print-div-table)
  (let loop ((i 1))
    (cond
      ((= i 16) #f)
      (else
        (let loop2 ((j 1))
          (cond
            ((= j 16) #f)
            (else
              (let ((expected (mod (quotient i j) 16))
                    (actual (uint->int (uint-div (int->uint i 4) (int->uint j 4)))))
                (if (= actual expected)
                  (print (format "~D / ~D = ~D" i j actual))
                  (print (format "Test failure ~D / ~D = ~D, but got ~D" i j expected actual)))
                (loop2 (+ j 1))))))
        (loop (+ i 1))))))

(print-div-table)
