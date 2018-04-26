(define (member-tree x tree)
  (let loop ((lis tree))
    (cond
      ((not (pair? lis)) (equal? x lis))
      ((null? lis) #f)
      ((member-tree x (car lis)) #t)
      (else
        (loop (cdr lis))))))
(print (member-tree 'd '(a (b (c (d . e) f) g) h)))
; #t
(print (member-tree 'e '#?=(a #?=(b #?=(c #?=(d . e) f) g) h)))
; #t
(print (member-tree 'x '#?=(a #?=(b #?=(c #?=(d . e) f) g) h)))
; #f
