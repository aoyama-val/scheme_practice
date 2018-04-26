(use file.util)

(define (last-item lis)
  (cond ((null? lis) (values #f '()))
        (else
          (let ((lis2 (reverse lis)))
            (values (car lis2) (reverse (cdr lis2)))))))

;(receive (a b) (last-item '(1 2 3 4 5))
         ;(print a)
         ;(print b))

(define (m dir)
  (print dir)
  (print (string-join (tree dir) "\n"))
  (print "")
  (print "n directories, n files")) ; nの表示は未実装

(define (list-files dir)
  (map (lambda (x) (string-append dir "/" x)) (remove (lambda (x) (member x '("." ".."))) (directory-list dir))))

(define (tree dir)
  (let ((files (list-files dir)))
    (apply append (map (lambda (x) (apply sub x)) (make-pair-with-last files)))))

(define (make-pair-with-last lis)
  (cond ((null? lis) '())
        (else
          (receive (last others) (last-item lis)
                   (append (map (lambda (x) (cons x '(#f))) others) (list (cons last '(#t))))))))

(define (get-line last?)
  (if last? "└── "  "├── "))

(define (get-line2 last?)
  (if last? "    " "│   "))

(define (append-line last? file)
  (string-append (get-line last?) (sys-basename file)))

(define (append-line2 last? file)
  (string-append (get-line2 last?) (sys-basename file)))

(define (sub file last?)
  (cond ((file-is-directory? file)
         (cons
           (append-line last? file)
           (map (lambda (x) (append-line2 last? x)) (apply append (map (lambda (x) (apply sub x)) (make-pair-with-last (list-files file)))))))
        (else (list (append-line last? file)))))

;#?=(sub ".")
;(print (sub "./tree.scm"))

(m "/Users/val00362/rosen_auth2/app")
