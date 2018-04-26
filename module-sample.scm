;; モジュールのサンプル
(define-module module-sample
               (use srfi-1)
               (export hoge foo)
               )

(select-module module-sample)

(define (hoge)
  (print "hoge"))

(define (foo)
  (print "foo"))

(define (bar)
  (print "bar"))


;; 使う側では以下のように書く
; (add-load-path ".")
;(use module-sample)
;(hoge)
