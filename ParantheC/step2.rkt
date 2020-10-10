#lang racket
(require "parenthec.rkt")

(define-program-counter pc)
(define fib-n #f)
(define fib-k #f)
(define apply-k-k #f)
(define apply-k-v #f)

(define-union continuations
  (init jump-out)
  (sub2 fib-sub1 k)
  (sub1 n k))

(define-label fib
  (cond
    [(< fib-n 2)
     (begin [set! apply-k-k fib-k]
            [set! apply-k-v 1]
            [set! pc apply-k])]
    [else
     (begin [set! fib-k (continuations_sub1 fib-n fib-k)]
            [set! fib-n (sub1 fib-n)]
            [set! pc fib])]))

(define-label apply-k
  (union-case apply-k-k continuations
    [(init jump-out) (dismount-trampoline jump-out)]
    [(sub2 fib-sub1 k)
     (begin [set! apply-k-k k]
            [set! apply-k-v (+ fib-sub1 apply-k-v)]
            [set! pc apply-k])]
    [(sub1 n k)
     (begin [set! fib-k (continuations_sub2 apply-k-v k)]
            [set! fib-n (sub1 (sub1 n))]
            [set! pc fib])]))

(define-label main
  (begin #;[set! fib-k (continuations_init)]
         [set! fib-n 5]
         [set! pc fib]
         [mount-trampoline continuations_init fib-k pc]
         (printf "~s\n" apply-k-v)))

(main)
