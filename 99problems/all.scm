(use srfi-1)

(define (flatten lis)
  (cond
    ((pair? lis)
     (apply append (map flatten lis)))
    (else
      (list lis))))

; 01.scm
(define (single? lis)
  (if (null? lis)
    #f
    (null? (cdr lis))))

; 02.scm
(define (double? lis)
  (if (null? lis)
    #f
    (single? (cdr lis))))


; 03.scm
(define (longer? xs ys)
  (cond
    [(null? xs) #f]
    [(null? ys) #t]
    [else (longer? (cdr xs) (cdr ys))]))

; 04.scm
(define (last lis)
  (list (car (reverse lis))))

(define (butlast lis)
  (reverse (cdr (reverse lis))))

; 05.scm
(define (take xs n)
  (let loop ((i 0) (lis xs) (ret '()))
    (cond
      ((null? lis) (reverse ret))
      ((= i n) (reverse ret))
      (else (loop (+ i 1) (cdr lis) (cons (car lis) ret))))))

; 06.scm
(define (drop lis n)
  (cond ((null? lis) '())
        ((= n 0) lis)
        (else (drop (cdr lis) (- n 1)))))

; 07.scm
(define (subseq xs n m)
  (take (drop xs n) (- m n)))

; 08.scm
(define (butlastn xs n)
  (reverse (drop (reverse xs) n)))

; 09.scm
(define (group lis n)
  (cond ((null? lis) '())
        (else (cons (take lis n) (group (drop lis n) n)))))

; 10.scm
(define (position x lis)
  (cond ((null? lis) #f)
        ((equal? x (car lis)) 0)
        (else (and (position x (cdr lis)) (+ 1 (position x (cdr lis)))))))

; 11.scm
(define (count x lis)
  (cond ((null? lis) 0)
        ((equal? x (car lis)) (+ 1 (count x (cdr lis))))
        (else (count x (cdr lis)))))

; 12.scm
(define (sum-list lis)
  (cond ((null? lis) 0)
        (else (+ (car lis) (sum-list (cdr lis))))))

; 13.scm
(define (max-list lis)
  (if (null? lis)
    #f
    (let loop ((lis (cdr lis)) (ret (car lis)))
      (if (null? lis)
        ret
        (if (> (car lis) ret)
          (loop (cdr lis) (car lis))
          (loop (cdr lis) ret))))))

(define (min-list lis)
  (if (null? lis)
    #f
    (let loop ((lis (cdr lis)) (ret (car lis)))
      (if (null? lis)
        ret
        (if (< (car lis) ret)
          (loop (cdr lis) (car lis))
          (loop (cdr lis) ret))))))

; 14.scm
(define (adjacent? x y lis)
  (cond
    ((null? lis) #f)
    ((and (not (null? (cdr lis))) (equal? (car lis) x) (equal? (cadr lis) y)) #t)
    (else (adjacent? x y (cdr lis)))))

(define (before? x y lis)
  (cond ((null? lis) #f)
        ((equal? x (car lis)) (member? y (cdr lis)))
        (else (before? x y (cdr lis)))))

(define (member? x lis)
  (cond ((null? lis) #f)
        ((equal? x (car lis)) #t)
        (else (member? x (cdr lis)))))

; 16.scm
(define (iota start end)
  (cond ((> start end) '())
        (else (cons start (iota (+ 1 start) end)))))

; 17.scm
(define (set-of-list lis)
  (cond
    ((null? lis) '())
    ((member (car lis) (cdr lis)) (set-of-list (cdr lis)))
    (else (cons (car lis) (set-of-list (cdr lis))))))

; 18.scm
(define (union a b)
  (cond
    ((null? a) b)
    ((member (car a) b) (union (cdr a) b))
    (else (cons (car a) (union (cdr a) b)))))

; 19.scm
(define (intersection a b)
  (cond
    ((null? a) '())
    ((member (car a) b) (cons (car a) (intersection (cdr a) b)))
    (else (intersection (cdr a) b))))

; 20.scm
(define (difference a b)
  (cond
    ((null? a) '())
    ((member (car a) b) (difference (cdr a) b))
    (else (cons (car a) (difference (cdr a) b)))))

; 21.scm
(define (merge-list comp a b)
  (sort (append a b) comp))

; 22.scm
(define (merge-list comp a b)
  (sort (append a b) comp))

(define (merge-sort comp a lis)
  (cond
    ((null? lis) '())
    ((comp a (car lis)) (merge-sort comp a (cdr lis)))
    (else (merge-list comp (list (car lis)) (merge-sort comp a (cdr lis))))))

; 23.scm
(define (prefix lis pref)
  (cond
    ((null? lis) #f)
    ((null? pref) #t)
    ((equal? (car lis) (car pref)) (prefix (cdr lis) (cdr pref)))
    (else #f)))

; 24.scm
(define (suffix lis suf)
  (prefix (reverse lis) (reverse suf)))

; 25.scm
(define (sublist sub lis)
  (cond
    ((null? sub) #t)
    ((null? lis) #f)
    ((equal? (take lis (length sub)) sub) #t)
    (else (sublist sub (cdr lis)))))

; 26.scm
(define (member-tree x tree)
  (let loop ((lis tree))
    (cond
      ((not (pair? lis)) (equal? x lis))
      ((null? lis) #f)
      ((member-tree x (car lis)) #t)
      (else
        (loop (cdr lis))))))

; 27.scm
; 再帰のみバージョン
(define (count-leaf tree)
  (cond
    ((null? tree) 0)
    ((pair? tree) (+ (count-leaf (car tree)) (count-leaf (cdr tree))))
    (else 1)))

; 28.scm
(define (subst a b tree)
  (cond
    ((null? tree) '())
    ((pair? tree)
     (cons (subst a b (car tree))
           (subst a b (cdr tree))))
    (else (if (equal? tree a)
            b
            tree))))

; 29.scm
(define (delete-nth lis n)
  (let loop ((i 0) (lis lis) (ret '()))
    (cond ((null? lis) ret)
          (else
            (loop (+ i 1) (cdr lis) (if (equal? i n)
                                      ret
                                      (append ret (list (car lis)))))))))

(define (nth lis n)
  (if (not (pair? lis))
    (error "not pair" lis)
  (let loop ((lis lis) (i 0))
    (cond
      ((null? lis) '())
      ((= i n) (car lis))
      (else (loop (cdr lis) (+ i 1)))))))

(define (permutation n lis)
  (let loop ((i 0) (ret '()))
    (cond
      ((null? lis) ret)
      ((= 1 (length lis)) (list lis))
      ((= i (length lis)) ret)
      (else (loop (+ i 1) (append ret (map (lambda (x) (cons (nth lis i) x)) (permutation (- n 1) (delete-nth lis i)))))))))

(define (repeat-perm n lis)
  (cond ((= n 0) '())
        ((= n 1) (map (lambda (x) (list x)) lis))
        (else (apply append (map (lambda (x) (map (lambda (y) (cons x y)) (repeat-perm (- n 1) lis))) lis)))))

; 31.scm
(define (comb-num n r)
  (/ (/ (factorial n) (factorial r)) (factorial (- n r))))

(define (factorial n)
  (if (= n 0)
    1
    (* n (factorial (- n 1)))))

; 32.scm
(define (combination n lis)
  (cond
    ((= n 1) (map (lambda (x) (list x)) lis))
    (else (apply append (map (lambda (x) (map (lambda (y) (cons x y)) (combination (- n 1) (cdr (member x lis))))) lis)))))

; 33.scm
(define (repeat-comb n lis)
  (cond
    ((= n 1) (map (lambda (x) (list x)) lis))
    (else (apply append (map
            (lambda (x) (map (lambda (y) (cons x y)) (repeat-comb (- n 1) (member x lis))))
            lis)))))

; 34.scm
(define (split-nth lis n)
  (cond
    ((null? lis) (values '() '()))
    (else (let loop ((i 0) (p1 '()) (rest lis))
      (cond
        ((= i n) (values p1 rest))
        (else (loop (+ i 1) (append p1 (list (car rest))) (cdr rest))))))))

; 35.scm
(define (partition lis)
  (let loop ((i 0) (lis lis) (odds '()) (evens '()))
    (cond
      ((null? lis) (values (reverse odds) (reverse evens)))
      ((= 0 (mod i 2)) (loop (+ i 1) (cdr lis) (cons (car lis) odds) evens))
      (else (loop (+ i 1) (cdr lis) odds (cons (car lis) evens))))))

; 36.scm
(define (split-find x lis)
  (let loop ((lis lis) (lis1 '()))
    (cond
      ((null? lis) (values (reverse lis1) '()))
      ((equal? (car lis) x) (values (reverse lis1) lis))
      (else (loop (cdr lis) (cons (car lis) lis1))))))

; 37.scm
(define (split-ge x lis)
  (values (filter (lambda (y) (>= y x)) lis)
          (filter (lambda (y) (< y x)) lis)))

; 38.scm
(define (pack lis)
  (cond
    ((null? lis) '())
    ((null? (cdr lis)) (list (list (car lis))))
    ((equal? (car lis) (cadr lis)) (let [(next (pack (cdr lis)))] (cons (cons (car lis) (car next)) (cdr next))))
    (else (cons (list (car lis)) (pack (cdr lis))))))

; 39.scm
(define (successor? a b)
  (= a (- b 1)))

(define (pack-num-list lis)
  (cond
    ((null? lis) '())
    ((null? (cdr lis)) (list (car lis)))
    ((successor? (car lis) (cadr lis))
     (let [(next (pack-num-list (cdr lis)))]
       (if (pair? (car next))
         (cons [cons (car lis) (cdar next)] (cdr next))
         (cons [cons (car lis) (car next)] (cdr next)))))
    (else (cons (car lis) (pack-num-list (cdr lis))))))

; 40.scm
(define (expand-pair pair)
  (iota (+ 1 (- (cdr pair) (car pair))) (car pair)))

(define (expand-num-list lis)
  (cond
    ((null? lis) '())
    ((pair? (car lis)) (append (expand-pair (car lis)) (expand-num-list (cdr lis))))
    (else (cons (car lis) (expand-num-list (cdr lis))))))


; 41.scm
(define (encode lis)
  (cond
    ((null? lis) '())
    ((null? (cdr lis)) (list (cons (car lis) 1)))
    ((equal? (car lis) (cadr lis)) 
     (let
        ((next (encode (cdr lis))))
        (cons (cons (car lis) (+ 1 (cdar next))) (cdr next))))
    (else
      (let
        ((next (encode (cdr lis))))
        (cons (cons (car lis) 1) next)))))

; 42.scm
(define (decode lis)
  (apply append (map (lambda (pair) (repeat (car pair) (cdr pair))) lis)))

(define (repeat x n)
  (let loop ((i 0) (result '()))
    (cond
      ((= i n) result)
      (else (loop (+ i 1) (cons x result))))))

; 43.scm
(define (any? pred lis)
  (cond
    ((null? lis) #f)
    ((pred (car lis)) #t)
    (else (any? pred (cdr lis)))))

(define (every? pred lis)
  (cond
    ((null? lis) #t)
    ((pred (car lis)) (every? pred (cdr lis)))
    (else #f)))

; 44.scm
(define (maplist f lis)
    (cond
      ((null? lis) '())
      (else (cons (f lis) (maplist f (cdr lis))))))

; 45.scm
(define (foldr f init lis)
  (cond
    ((null? lis) init)
    (else (f (car lis) (foldr f init (cdr lis))))))

(define (for-each-list fn comb term xs)
  (foldr comb term (map fn xs)))

; 46.scm
(define (mymap f lis)
  (for-each-list f cons '() lis))

(define (myfilter f lis)
  (apply append (for-each-list (lambda (x) (if (f x) (list x) '())) cons '() lis)))

(define (myfold f init lis)
  (for-each-list (lambda (x) x) f init lis))

; 47.scm
(define (komachi)
  (let loop
    [(permlist (permutation 9 '(1 2 3 4 5 6 7 8 9)))
     (result '())]
    (cond
      ((null? permlist) (reverse result))
      ((answer? (car permlist)) (loop (cdr permlist) (cons (car permlist) result)))
      (else (loop (cdr permlist) result)))))

(define (calc-aux a b c)
  (/ a (+ (* 10 b) c)))

(define (calc p)
  (+
    (calc-aux (nth p 0) (nth p 1) (nth p 2))
    (calc-aux (nth p 3) (nth p 4) (nth p 5))
    (calc-aux (nth p 6) (nth p 7) (nth p 8))))

(define (answer? lis)
  (= 1 (numerator (calc lis))))

; 48.scm
(define (all=? lis)
  (cond
    ((null? lis) #t)
    (else (every (lambda (x) (= x (car lis))) lis))))

(define (get-nths lis nths)
  (map (lambda (n) (nth lis n)) nths))

(define (answer-48? lis)
  (=
    (apply + (get-nths lis '(0 1 2)))
    (apply + (get-nths lis '(3 4 5)))
    (apply + (get-nths lis '(6 7 8)))
    (apply + (get-nths lis '(0 3 6)))
    (apply + (get-nths lis '(1 4 7)))
    (apply + (get-nths lis '(2 5 8)))
    (apply + (get-nths lis '(0 4 8)))
    (apply + (get-nths lis '(2 4 6)))))

(define (solve-48)
  (let loop ((perms (permutation (iota 9 1))) (result '()))
    (cond
      ((null? perms) result)
      ((answer-48? (car perms)) (loop (cdr perms) (cons (car perms) result)))
      (else (loop (cdr perms) result)))))

(define (list-to-num lis)
  (let loop ((lis lis) (result 0))
    (cond
      ((null? lis) result)
      (else
        (loop (cdr lis) (+ (* result 10) (car lis)))))))

(define (slice lis start count)
  (take (drop lis start) count))

;("W" "R" "O" "N" "G" "M" "I" "H" "T")
(define all-chars (delete-duplicates (map symbol->string '(W R O N G M R I G H T))))

(define (list-index e lis)
  (if (null? lis)
    -1
    (if (equal? (car lis) e)
      0
      (if (= (list-index e (cdr lis)) -1) 
        -1
        (+ 1 (list-index e (cdr lis)))))))

(define (strrep->indexes s)
  (map (lambda (c) (charrep->index (list->string (list c)))) (string->list s)))

(define (charrep->index c)
  (list-index c all-chars))

(define (strrep->num lis s)
  (list-to-num (map (lambda (i) (nth lis i)) (strrep->indexes s))))

(define (answer-49? lis)
  (=
    (* (strrep->num lis "WRONG") (strrep->num lis "M"))
    (strrep->num lis "RIGHT")))

(define (solve-49)
  (filter answer-49? (permutation (iota 9 1))))

; 50.scm
(define (sieve n)
  (let loop ((result '()) (x 2))
    (cond
      ((> x n) (reverse result))
      ((any (lambda (a) (= a 0)) (map (lambda (p) (mod x p)) result))
       (loop result (+ x 1)))
      (else
        (loop (cons x result) (+ x 1))))))

; 51.scm
(define (xor p q)
  (or
    (and (not p) q)
    (and p (not q))))

; 52.scm
(define (half-adder p q)
  (values
    (xor p q)
    (and p q)))

; 53.scm
(define (full-adder p q r)
  (receive (s c) (half-adder p q)
           (values
             (xor s r)
             (or
               (and p q)
               (and r (xor p q))))))

; 54.scm
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

; 55.scm
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
  (map (lambda (x) (apply bool-and x)) (zip i1 i2)))

(define (uint-or i1 i2)
  (map (lambda (x) (apply bool-or x)) (zip i1 i2)))

(define (uint-xor i1 i2)
  (map (lambda (x) (apply bool-xor x)) (zip i1 i2)))

(define (uint-not i1)
  (map not i1))

; 56.scm
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


; 57.scm
(define (uint-inc i1)
  (uint-add i1 (append (make-list (- (length i1) 1) #f) (list #t))))

; 58.scm
(define (uint-neg x)
  (uint-inc (uint-not x)))

(define (uint-sub x y)
  (receive (s c) (uint-add x (uint-neg y))
           (values
             s
             (not c))))

; 59.scm
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

; 60.scm
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

(define (int->uint n m)
  (cond
    ((= m 0) '())
    (else
      (append (int->uint (quotient n 2) (- m 1))
               (list (if (= (mod n 2) 0) #f #t))))))


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
    ((not (= (length i1) (length i2))) (error (format "uint-add length not equal ~D ~D" (length i1) (length i2)))) ; 長さが違うと見つけにくいバグになるのでエラーを出すようにする
    ((null? i1) (values i2 #f))
    ((null? i2) (values i1 #f))
    (else
      (receive (next-sum next-carry) (uint-add (cdr i1) (cdr i2))
               (receive (s c) (full-adder (car i1) (car i2) next-carry)
                        (values
                          (cons s next-sum)
                          c))))))



(define (get-opcode command)
  (take command 3))

(define (get-operand command)
  (drop command 3))

(define (command-jump operand)
  (if (= (uint->int of) 1)
    #f
    (set! pc (uint-sub operand (int->uint 1 (length pc))))))  ; 自動PCインクリメントにより1増加するから、ここで-1しておく

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
  (print "not defined"))

(define (command-srl operand)
  (print "not defined"))

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


;; 仮想マシン実行開始
(define (run)
  (exec-and-increment-pc)
  (run))

; 終了地点を保存しておく
(define exit-cont '())
(call/cc (lambda (cont)
  (set! exit-cont cont)))


;(run)

; 67.scm
(define (comp x y) (- x y))

(define (min-vector cp vec start end)
  (let loop ((i start) (min (vector-ref vec start)) (min-index start))
    (cond
      ((>= i end) min-index)
      ((negative? (cp (vector-ref vec i) min)) (loop (+ i 1) (vector-ref vec i) i))
      (else (loop (+ i 1) min min-index)))))

(define (select-sort-vector cp vec)
  (let loop ((i 0))
    (cond
      ((>= i (vector-length vec)) vec)
      (else
        (let* ((min-index (min-vector cp vec i (vector-length vec))) (tmp (vector-ref vec i)))
          (vector-set! vec i (vector-ref vec min-index))
          (vector-set! vec min-index tmp)
          (loop (+ i 1)))))))

; 68.scm
(define (quick-sort-vector cp vec)
  )

; 69.scm
(define (rpn lis)
  (rpn-aux lis '()))

(define (rpn-aux lis stack)
  (cond
    ((null? lis) (car stack))
    ((equal? (car lis) '+) (rpn-aux (cdr lis) (cons (+ (cadr stack) (car stack)) (cddr stack))))
    ((equal? (car lis) '-) (rpn-aux (cdr lis) (cons (- (cadr stack) (car stack)) (cddr stack))))
    ((equal? (car lis) '*) (rpn-aux (cdr lis) (cons (* (cadr stack) (car stack)) (cddr stack))))
    ((equal? (car lis) '/) (rpn-aux (cdr lis) (cons (/ (cadr stack) (car stack)) (cddr stack))))
    ((number? (car lis)) (rpn-aux (cdr lis) (cons (car lis) stack)))
    (else
      (car lis))))


; 70.scm
(define (expression lis)
  (parse-add lis))

(define (parse-number lis)
  (cond
    ((number? (car lis)) (values (car lis) (cdr lis)))
    ((list? (car lis)) (parse-add (car lis)))
    (else
      (error "parse-number error" (car lis)))))

(define (parse-mul lis)
  (let loop ((result (parse-number lis)) (lis (cdr lis)))
    (cond
      ((null? lis) (values result '()))
      ((equal? (car lis) '*)
       (loop (* result (parse-number (cdr lis))) (cddr lis)))
      ((equal? (car lis) '/)
       (loop (/ result (parse-number (cdr lis))) (cddr lis)))
      (else
        (values result lis)))))

(define (parse-add lis)
  (receive (t1 rest1) (parse-mul lis)
           (let loop ((result t1) (rest rest1))
             (cond
               ((null? rest) result)
               ((equal? (car rest) '+)
                (receive (term rest2) (parse-mul (cdr rest))
                         (loop (+ result term) rest2)))
               ((equal? (car rest) '-)
                (receive (term rest2) (parse-mul (cdr rest))
                         (loop (- result term) rest2)))
               (else
                 (error "parse-mul error" lis))))))
; 71.scm
(define (prefix->postfix lis)
  (if (pair? lis)
    (case (car lis)
      ((*) (append (prefix->postfix (cadr lis)) (prefix->postfix (caddr lis)) (list '*)))
      ((+) (append (prefix->postfix (cadr lis)) (prefix->postfix (caddr lis)) (list '+)))
      ((-) (append (prefix->postfix (cadr lis)) (prefix->postfix (caddr lis)) (list '-)))
      ((/) (append (prefix->postfix (cadr lis)) (prefix->postfix (caddr lis)) (list '/)))
      (else (list (car lis))))
    (list lis)))

;72.scm
(define (postfix->prefix lis)
  (let loop ((lis lis) (stack '()))
    (cond
      ((null? lis) stack)
      (else
        (case (car lis)
          ((+ - * /)
           (loop (cdr lis) (cons (list (car lis) (cadr stack) (car stack)) (cddr stack))))
          (else
            (loop (cdr lis) (cons (car lis) stack))))))))


; 74.scm
(define (flatexpr lis)
  (cond
    ((not (pair? lis)) (list lis))
    ((equal? (nth lis 1) '+)
     (if (>= 1 (get-expr-priority lis))
       (apply append (list (flatexpr (car lis)) (list '+) (flatexpr (nth lis 2))))
       lis))
    ((equal? (nth lis 1) '*)
       (apply append
          (list
            (if (= 2 (get-expr-priority (car lis)))
              (flatexpr (car lis))
              (list (car lis)))
            '(*)
            (if (= 2 (get-expr-priority (nth lis 2)))
              (flatexpr (nth lis 2))
              (list (nth lis 2))))))
    (else
      "not defined")))

(define (get-priority sym)
  (case sym
    ((+ -) 1)
    ((* /) 2)
    (else 0)))

(define (get-expr-priority lis)
  (if (pair? lis)
    (get-priority (cadr lis))
    (get-priority lis)))

