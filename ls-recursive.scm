#!/usr/bin/env gosh

;;; treeコマンド

(define (string-repeat str n)
  (cond
    ((= n 0) "")
    (else
      (string-append str (string-repeat str (- n 1))))))

(define (make-indent n)
  (string-repeat " " (* n 4)))

(use file.util)

(define (equal-any? x lis compare)
  (any (lambda (y) (compare y x)) lis))

(define (tree dir indent)
  (receive (dirs files) (directory-list2 dir)
           (map (lambda (x) (print (make-indent indent) x)) files)
           (map (lambda (x) (print (make-indent indent) x "/") (tree (string-append dir "/" x) (+ 1 indent))) (filter (lambda (x) (not (equal-any? x '("." "..") string=?))) dirs))))

(define (main args)
  (cond
    ((= 1 (length args))
     (tree "." 0))
    (else
      (tree (cadr args) 0))))

;(tree "." 0)
