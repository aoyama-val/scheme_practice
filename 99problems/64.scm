;(load "./all.scm")

(define (comp x y) (- x y))

(define (smaller? a b)
  (negative? (comp a b)))

(define (merge-list! comp xs ys)
  (let ((header (list #f)))
    (let loop ((xs xs) (ys ys) (tail header))
      (cond ((null? xs)
             (set-cdr! tail ys)
             (cdr header))
            ((null? ys)
             (set-cdr! tail xs)
             (cdr header))
            ((positive? (comp (car xs) (car ys)))
             (set-cdr! tail ys)
             (loop xs (cdr ys) ys))
            (else
             (set-cdr! tail xs)
             (loop (cdr xs) ys xs))))))

(define a '())
(define b '(1 4 6))

(print (merge-list! comp a b))
;(1 4 6)

(print a)
;()

(print b)
;(1 4 6)

(exit)


; comp
(print (merge-list! comp '(1 3 5 7) '(2 4 6 8)))

; (1 2 3 4 5 6 7 8)
(print (define a '(1 3 5 7 9)))
; a
(print (define b '(0 2 4 6 8)))
; b
(print (merge-list! comp a b))
; (0 1 2 3 4 5 6 7 8 9)
(print a)
; (1 2 3 4 5 6 7 8 9)
(print b)
; (0 1 2 3 4 5 6 7 8 9)

