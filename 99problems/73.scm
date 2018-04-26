(define (prefix->infix lis)
  (if (pair? lis)
    (case (car lis)
      ((*) (list (prefix->infix (cadr lis)) '* (prefix->infix (caddr lis))))
      ((+) (list (prefix->infix (cadr lis)) '+ (prefix->infix (caddr lis))))
      ((-) (list (prefix->infix (cadr lis)) '- (prefix->infix (caddr lis))))
      ((/) (list (prefix->infix (cadr lis)) '/ (prefix->infix (caddr lis))))
      (else (car lis)))
    lis))



(print (prefix->infix 4))
(print (prefix->infix '(* 2 3)))

(print (prefix->infix '(* (+ 1 2) (- 3 4))))
; (1 2 + 3 4 - *)
(print (prefix->infix '(* (+ 1 2) (- 3 (/ 4 5)))))
; (1 2 + 3 4 5 / - *)
