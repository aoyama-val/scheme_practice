(load "./all.scm")



(print (flatexpr '((1 + 2) + (3 + 4))))
(print (flatexpr '((1 + 2) + (3 * 4))))
(print (flatexpr '((1 * 2) + (3 + 4))))
(print (flatexpr '((1 * 2) + (3 * 4))))
(print (flatexpr '((1 + 2) * (3 + 4))))
(print (flatexpr '((1 + 2) * (3 * 4))))
(print (flatexpr '((1 * 2) * (3 + 4))))
(print (flatexpr '((1 * 2) * (3 * 4))))
