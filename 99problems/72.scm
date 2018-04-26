(load "./all.scm")

(print (postfix->prefix '(1 2 + 3 4 - *)))
; (* (+ 1 2) (- 3 4))
(print (postfix->prefix '(1 2 + 3 4 5 / - *)))
; (* (+ 1 2) (- 3 (/ 4 5)))
