(define (split-ge x lis)
  (values (filter (lambda (y) (>= y x)) lis)
          (filter (lambda (y) (< y x)) lis)))

(receive (a b) (split-ge 5 '(4 6 3 5 7 8 2 9 1)) (print a b))
; (4 3 2 1)
; (6 5 7 8 9)
