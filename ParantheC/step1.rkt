#lang racket
(require "parenthec.rkt")

(define fib-n #f)
(define fib-k #f)
(define apply-k-k #f)
(define apply-k-v #f)

(define-union continuations
  (init)
  (sub2 fib-sub1 k)
  (sub1 n k))

#|for the union of environments, you need two (empty-env and ext-env)|#
#|for the union of closures, you need one|#
#|for the union of expressions, you need a lot|#

(define fib
  (λ () #|n k|#
    (cond
      [(< fib-n 2)
       (begin [set! apply-k-k fib-k]
              [set! apply-k-v 1]
              (apply-k))]
      [else
       (begin [set! fib-k (continuations_sub1 fib-n fib-k)]
              [set! fib-n (sub1 fib-n)]
              (fib))])))

(define apply-k
  (λ () #|k v|#
    (union-case apply-k-k continuations
      [(init) apply-k-v]
      [(sub2 fib-sub1 k)
       (begin [set! apply-k-k k]
              [set! apply-k-v (+ fib-sub1 apply-k-v)]
              (apply-k))]
      [(sub1 n k)
       (begin [set! fib-k (continuations_sub2 apply-k-v k)]
              [set! fib-n (sub1 (sub1 n))]
              (fib))])))

#;
(define make-k-sub1
  #|simple|#
  (λ (n k)
    `(sub1 ,n ,k)))

#;
(define make-k-sub2
  #|simple|#
  (λ (fib-sub1 k)
    `(sub2 ,fib-sub1 ,k)))

#;
(define make-k-init
  #|simple|#
  (λ ()
    ))

(begin [set! fib-k (continuations_init)]
       [set! fib-n 5]
  (fib))

