(define (prefix->postfix lis)
  (if (pair? lis)
    (case (car lis)
      ((*) (append (prefix->postfix (cadr lis)) (prefix->postfix (caddr lis)) (list '*)))
      ((+) (append (prefix->postfix (cadr lis)) (prefix->postfix (caddr lis)) (list '+)))
      ((-) (append (prefix->postfix (cadr lis)) (prefix->postfix (caddr lis)) (list '-)))
      ((/) (append (prefix->postfix (cadr lis)) (prefix->postfix (caddr lis)) (list '/)))
      (else (list (car lis))))
    (list lis)))



(print (prefix->postfix 4))
(print (prefix->postfix '(* 2 3)))

(print (prefix->postfix '(* (+ 1 2) (- 3 4))))
; (1 2 + 3 4 - *)
(print (prefix->postfix '(* (+ 1 2) (- 3 (/ 4 5)))))
; (1 2 + 3 4 5 / - *)
