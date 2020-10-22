#lang racket

(define fib
  (λ (n k)
    (cond
      [(< n 2) (apply-k k 1)]
      [else (fib (sub1 n)
                 (make-k-sub1 n k))])))

(define apply-k
  (λ (k v)
    (match k
      [`(init) v]
      [`(sub2 ,fib-sub1 ,k) (apply-k k (+ fib-sub1 v))]
      [`(sub1 ,n ,k) (fib (sub1 (sub1 n))
                          (make-k-sub2 v k))])))

(define make-k-sub1
  (λ (n k)
    `(sub1 ,n ,k)))

(define make-k-sub2
  (λ (fib-sub1 k)
    `(sub2 ,fib-sub1 ,k)))

(define make-k-init
  (λ ()
    `(init)))

(fib 5 (make-k-init))
