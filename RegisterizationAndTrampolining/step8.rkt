#lang racket

#|
introducing registers for serious functions
|#

(define fib-n #f)
(define fib-k #f)
(define apply-k-k #f)
(define apply-k-v #f)

(define fib
  (λ () #|n k|#
    (cond
      [(< fib-n 2)
       (begin [set! apply-k-k fib-k]
              [set! apply-k-v 1]
              (apply-k))]
      [else
       (begin [set! fib-k (make-k-sub1 fib-n fib-k)]
              [set! fib-n (sub1 fib-n)]
              (fib))])))

(define apply-k
  (λ () #|k v|#
    (match apply-k-k
      [`(init) apply-k-v]
      [`(sub2 ,fib-sub1 ,k)
       (begin [set! apply-k-k k]
              [set! apply-k-v (+ fib-sub1 apply-k-v)]
              (apply-k))]
      [`(sub1 ,n ,k)
       (begin [set! fib-k (make-k-sub2 apply-k-v k)]
              [set! fib-n (sub1 (sub1 n))]
              (fib))])))

(define make-k-sub1
  #|simple|#
  (λ (n k)
    `(sub1 ,n ,k)))

(define make-k-sub2
  #|simple|#
  (λ (fib-sub1 k)
    `(sub2 ,fib-sub1 ,k)))

(define make-k-init
  #|simple|#
  (λ ()
    `(init)))

(begin [set! fib-k (make-k-init)]
       [set! fib-n 5]
  (fib))

