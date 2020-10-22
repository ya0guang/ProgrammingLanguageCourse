#lang racket

#|
A-normal form
|#

(define fib
  (λ (n k)
    (cond
      [(< n 2)
       (let* ([k k]
              [v 1])
         (apply-k k v))]
      [else
       (let* ([k (make-k-sub1 n k)]
              [n (sub1 n)])
         (fib n k))])))

(define apply-k
  (λ (k v)
    (match k
      [`(init) v]
      [`(sub2 ,fib-sub1 ,k)
       (let* ([k k]
              [v (+ fib-sub1 v)])
         (apply-k k v))]
      [`(sub1 ,n ,k)
       (let* ([k (make-k-sub2 v k)]
              [n (sub1 (sub1 n))])
         (fib n k))])))

(define make-k-sub1
  (λ (n k)
    `(sub1 ,n ,k)))

(define make-k-sub2
  (λ (fib-sub1 k)
    `(sub2 ,fib-sub1 ,k)))

(define make-k-init
  (λ ()
    `(init)))

(let* ([k (make-k-init)]
       [n 5])
  (fib n k))

