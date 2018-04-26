; 全部で９文字
;gosh> (delete-duplicates (map symbol->string '(W R O N G M R I G H T)))
;("W" "R" "O" "N" "G" "M" "I" "H" "T")

(define (nth lis n)
  (let loop ((lis lis) (i 0))
    (cond
      ((null? lis) '())
      ((= i n) (car lis))
      (else (loop (cdr lis) (+ i 1))))))

(define (permutation lis)
  (define (delete-nth n lis)
    (let loop ((i 0) (lis lis) (forward '()))
      (cond
        ((equal? i n) (values (append (reverse forward) (cdr lis)) (car lis)))
        (else (loop (+ i 1) (cdr lis) (cons (car lis) forward))))))
  (if (null? lis)
    '(())
    (let loop ((i 0) (result '()))
      (cond
        ((= i (length lis)) result)
        (else
          (loop (+ i 1)
                (append result (receive (deleted nth) (delete-nth i lis) (map (lambda (x) (cons nth x)) (permutation deleted))))))))))


;(define (digits-to-num lis digits)
  ;(let loop ((digits digits) (result 0))
    ;(cond
      ;((null? digits) result)
      ;(else
        ;(loop (cdr digits)
              ;(+ (* 10 result)
                 ;(nth lis (car digits))))))))

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

;(print (charrep->index "W"))
;(print (strrep->indexes "WRONG"))
;(print (strrep->indexes "M"))
;(print (strrep->indexes "RIGHT"))

(define (strrep->num lis s)
  (list-to-num (map (lambda (i) (nth lis i)) (strrep->indexes s))))

(print (strrep->num (iota 9 1) "WRONG"))
(print (strrep->num (iota 9 1) "M"))
(print (strrep->num (iota 9 1) "RIGHT"))

(define (answer? lis)
  (=
    (* (strrep->num lis "WRONG") (strrep->num lis "M"))
    (strrep->num lis "RIGHT")))

;(print (answer? '(3 1 4 1 5 9 2 6 5)))

;(list-to-num (slice lis 0 5))
;(list-to-num (slice lis 5 1))
;(list-to-num (slice lis 5 1))


(define (solve-49)
  (filter answer? (permutation (iota 9 1))))

(print "---------------\n")
(print (solve-49))
