(use srfi-1)

(define (set-nth lis n x)
  (append (take lis n) (list x) (drop lis (+ n 1))))

(define (make-word)
  '(#f #f #f #f #f #f #f #f))

;;
(define acc (make-word))
(define pc '(#f #f #f #f #f))
(define of '(#f))

(define (print-memory)
  (print "acc:" (map bool->int acc))
  (print "pc:" (map bool->int pc))
  (print "of" (map bool->int of))
  (let loop ((i 0) (lis memory))
    (cond
      ((null? lis) '())
      (else
        (begin
          (print (if (< i 10) (string-append "0" (number->string i)) i) ":" (map bool->int (car lis)))
          (loop (+ i 1) (cdr lis)))))))

(define (bool->int b)
  (cond
    ((list? b) (map bool->int b))
    (b 1)
    (else 0)))

(define (int->bool i)
  (if (= i 1)
    #t
    #f))

;; メモリ
(define memory 
  (list
    ; program １〜n（標準入力から読み込んだ値）を計算するプログラム
    (map int->bool '(1 1 1 0 0 0 0 1))  ; read
    (map int->bool '(1 0 0 1 1 1 0 1))  ; store to cnt
    (map int->bool '(0 1 1 1 1 1 1 0))  ; load from result
    (map int->bool '(0 0 1 1 1 1 0 1))  ; add cnt
    (map int->bool '(1 0 0 1 1 1 1 0))  ; store to result
    (map int->bool '(0 1 1 1 1 1 0 1))  ; load from cnt
    (map int->bool '(0 1 0 1 1 1 0 0))  ; sub 1
    (map int->bool '(0 0 0 0 0 0 0 1))  ; jump

    (map int->bool '(0 1 1 1 1 1 1 0))  ; load from result
    (map int->bool '(1 1 1 0 0 0 1 0))  ; print
    (map int->bool '(1 1 1 0 0 0 0 0))  ; exit
    (map int->bool '(0 0 0 0 0 0 0 0))
    (map int->bool '(0 0 0 0 0 0 0 0))
    (map int->bool '(0 0 0 0 0 0 0 0))
    (map int->bool '(0 0 0 0 0 0 0 0))
    (map int->bool '(0 0 0 0 0 0 0 0))

    (map int->bool '(0 0 0 0 0 0 0 0))
    (map int->bool '(0 0 0 0 0 0 0 0))
    (map int->bool '(0 0 0 0 0 0 0 0))
    (map int->bool '(0 0 0 0 0 0 0 0))
    (map int->bool '(0 0 0 0 0 0 0 0))
    (map int->bool '(0 0 0 0 0 0 0 0))
    (map int->bool '(0 0 0 0 0 0 0 0))
    (map int->bool '(0 0 0 0 0 0 0 0))

    (map int->bool '(0 0 0 0 0 0 0 0))
    (map int->bool '(0 0 0 0 0 0 0 0))
    (map int->bool '(0 0 0 0 0 0 0 0))
    (map int->bool '(0 0 0 0 0 0 0 0))

    ; constants
    (map int->bool '(0 0 0 0 0 0 0 1))  ; 11100 1
    (map int->bool '(0 0 0 0 0 0 0 0))  ; 11101 cnt
    (map int->bool '(0 0 0 0 0 0 0 0))  ; 11110 result
    (map int->bool '(0 0 0 0 0 0 0 0))  ; 11111
    ))

(if (= 32 (length memory))
  (print "memory check OK")
  (begin
    (print "memory check NG!")
    (exit)))
          


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

(define (bool-and a b)
  (if (and a b)
    #t
    #f))

(define (bool-or a b)
  (if (or a b)
    #t
    #f))

(define (bool-xor p q)
  (or
    (and (not p) q)
    (and p (not q))))

(define (uint-and i1 i2)
  (if (not (= (length i1) (length i2)))
    (error "uint-and length not equal")
    (map (lambda (x) (apply bool-and x)) (zip i1 i2))))

(define (uint-or i1 i2)
  (map (lambda (x) (apply bool-or x)) (zip i1 i2)))

(define (uint-xor i1 i2)
  (map (lambda (x) (apply bool-xor x)) (zip i1 i2)))

(define (uint-not i1)
  (map not i1))

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
    ((not (= (length i1) (length i2))) (error "uint-add length not equal")) ; 長さが違うと見つけにくいバグになるのでエラーを出すようにする
    ((null? i1) (values i2 #f))
    ((null? i2) (values i1 #f))
    (else
      (receive (next-sum next-carry) (uint-add (cdr i1) (cdr i2))
               (receive (s c) (full-adder (car i1) (car i2) next-carry)
                        (values
                          (cons s next-sum)
                          c))))))

(define (uint-inc i1)
  (uint-add i1 (append (make-list (- (length i1) 1) #f) (list #t))))

(define (uint-neg x)
  (uint-inc (uint-not x)))

(define (uint-sub x y)
  (if (not (= (length x) (length y)))
    (error "uint-sub length not equal")
    (receive (s c) (uint-add x (uint-neg y))
           (values
             s
             (not c)))))

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


(define (nth lis n)
  (let loop ((lis lis) (i 0))
    (cond
      ((null? lis) '())
      ((= i n) (car lis))
      (else (loop (cdr lis) (+ i 1))))))

(define (get-opcode command)
  (take command 3))

(define (get-operand command)
  (drop command 3))

(define (command-jump operand)
  (if (= (uint->int of) 1)
    #f
    (set! pc (uint-sub operand (int->uint 1 (length pc))))))  ; 自動PCインクリメントにより1増加するから、ここで-1しておく
  ; 模範解答によるとフェッチした段階でインクリメントするようにすればいいらしい

(define (command-add operand)
    (receive (s c)
             (uint-add acc (nth memory (uint->int operand)))
             (set! acc s)
             (set! of (list c))))

(define (command-sub operand)
    (receive (s c)
             (uint-sub acc (nth memory (uint->int operand)))
             (set! acc s)
             (set! of (list c))))

(define (command-load operand)
  (let ((n (uint->int operand)))
    (set! acc (nth memory n))))

(define (command-store operand)
  (let ((n (uint->int operand)))
    (set! memory (set-nth memory n acc))))

(define (command-sll operand)
  (receive (s c)
            (uint-sll acc)
            (set! acc s)
            (set! of (list c))))

(define (command-srl operand)
  (receive (s c)
            (uint-srl acc)
            (set! acc s)
            (set! of (list c))))

(define (command-svc operand)
  (cond
    ((= (uint->int operand) 0) (exit-cont)) ; exit
    ((= (uint->int operand) 1) (set! acc (int->uint (read) 8))) ; read
    ((= (uint->int operand) 2) (print "RESULT = " (uint->int acc)))  ; print
    (error "invalid operand")))

(define (exec-command-at addr)
  (exec-command (nth memory (uint->int addr))))

(define (exec-and-increment-pc)
  (print "PC = " (bool->int pc))
  (exec-command-at pc)
    (increment-pc!))

(define (exec-command command)
  (print "acc = " (bool->int acc))
  (print "exec " (bool->int command))
  (let ((opcode (get-opcode command)) (operand (get-operand command)))
    (cond
      ((equal? opcode '(#f #f #f)) (command-jump operand))
      ((equal? opcode '(#f #f #t)) (command-add operand))
      ((equal? opcode '(#f #t #f)) (command-sub operand))
      ((equal? opcode '(#f #t #t)) (command-load operand))
      ((equal? opcode '(#t #f #f)) (command-store operand))
      ((equal? opcode '(#t #f #t)) (command-sll operand))
      ((equal? opcode '(#t #t #f)) (command-srl operand))
      ((equal? opcode '(#t #t #t)) (command-svc operand))
      (else
        (error "invalid command")))))

(define (increment-pc!)
  (set! pc (uint-add pc '(#f #f #f #f #t))))

(define (times n proc)
  (let loop ((i 0))
    (cond
      ((= i n) '())
      (else
        (begin
          (proc)
        (loop (+ i 1)))))))


; 終了地点を保存しておく
(define exit-cont '())
(call/cc (lambda (cont)
  (set! exit-cont cont)))

;; 仮想マシン実行開始
(define (run)
  (exec-and-increment-pc)
  (run))

;(print (map bool->int (int->uint 100 8)))
;(print (map bool->int (int->uint 41 8)))
;(print (map bool->int (int->uint 59 8)))
;(print (map bool->int (uint-sub (int->uint 100 8) (int->uint 41 8))))
;(print (map bool->int (uint-not (int->uint 41 8))))
;(print (map bool->int (uint-neg (int->uint 41 8))))
;(exit)

;(print (bool->int (uint-sub (int->uint 100 8) (int->uint 1 8))))
;(exit)


;(print-memory)
(run)
(print "after------------------------------")
(print-memory)
