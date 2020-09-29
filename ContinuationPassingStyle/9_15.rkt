#lang racket
(require racket/trace)

#|
Continuation-passing Style
|#

(define !
  (λ (n)
    (cond
      [(zero? n) 1]
      [else
       (* n (! (sub1 n)))])))

(define !-cps
  (λ (n k)
    (cond
      [(zero? n) (k 1)]
      [else (!-cps (sub1 n)
               (λ (v) (k (* n v))))])))



(! 4 (λ (v) ((λ (v) v) (* 5 v))))
---------------

(trace !)
(! 5)
(trace !-cps)
(!-cps 5 (λ (v) v))

(define fib
  (λ (n)
    (cond
      [(< n 2) 1]
      [else (+ (fib (sub1 n)) (fib (sub1 (sub1 n))))])))

(trace fib)
#;
(fib 5)

(define fib-cps
  (λ (n k)
    (cond
      [(< n 2) (k 1)]
      [else (fib-cps (sub1 n)
                     (λ (fib-sub-1)
                       (fib-cps (sub1 (sub1 n))
                                (λ (fib-sub-2)
                                  (k (+ fib-sub-1 fib-sub-2))))))])))

(trace fib-cps)
#;
(fib-cps 5 (λ (v) v))

#;
(define Ω
  ((λ (x) (x x))
   (λ (x) (x x))))

(define purge
  (λ (f l)
    (cond
      [(null? l) '()]
      [(f (car l)) (purge f (cdr l))]
      [else (cons (car l) (purge f (cdr l)))])))

(define purge-cps
  (λ (f-cps l k)
    (cond
      [(null? l) (k '())]
      [else
       (f-cps (car l)
              (λ (b)
                (cond
                  [b (purge-cps f-cps (cdr l) k)]
                  [else (purge-cps f-cps (cdr l)
                                   (λ (d)
                                     (k (cons (car l) d))))])))])))

(define make-list
  (λ (n)
    (cond
      [(zero? n) '(0)]
      [else (cons n (make-list (sub1 n)))])))

(trace purge)
#;
(purge even? (make-list 50))

(trace purge-cps)
#;
(purge-cps (λ (v k) (k (even? v))) (make-list 100) (λ (v) v))


