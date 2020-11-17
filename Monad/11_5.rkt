#lang racket
(require "monads_class.rkt")

(define fib/state
  (λ (n)
    (bind-state (get)
                (λ (table)
                  (cond
                    [(assv n table)
                     =>
                     (λ (pr)
                       (inj-state (cdr pr)))]
                    [(zero? n) (inj-state 0)]
                    [(zero? (sub1 n)) (inj-state 1)]
                    [else
                     (bind-state (fib/state (sub1 n))
                                 (λ (fib-sub₁)
                                   (bind-state (fib/state (sub1 (sub1 n)))
                                               (λ (fib-sub₂)
                                                 (let ([result (+ fib-sub₁ fib-sub₂)])
                                                   (bind-state (get)
                                                               (λ (table)
                                                                 (bind-state (put `((,n . ,result) . ,table))
                                                                             (λ (_) (inj-state result))))))))))])))))

#;
(define fib
  (λ (n)
    (go-on ([table (get)])
      (cond
        [(assv n table)
         =>
         (λ (pr)
           (inj-state (cdr pr)))]
        [(zero? n) (inj-state 0)]
        [(zero? (sub1 n)) (inj-state 1)]
        [else
         (go-on ([fib-sub₁ (fib (sub1 n))]
                 [fib-sub₂ (fib (sub1 (sub1 n)))])
           (let ([result (+ fib-sub₁ fib-sub₂)])
             (go-on ([table (get)]
                     [_ (put `((,n . ,result) . ,table))])
               (inj-state result))))]))))

#|
state monads  : (State A S)
- injection  (put something at the value part) (→ A (State A S))
- bind (always bind a monads with a function that returns another monad) (→ (State A S) (→ A (State B S)) (State B S))
- get  (take the effect part)  : (→ (State A S) S)
- put  (put something at the effect part) (→ S (State A S))
|#

#|
 the 3 monads laws
- left-id: (bind (inj a) f) ≡ (f a)
- right-id: (bind ma inj) ≡ ma
- associative: (bind (bind ma f) g) ≡ (bind ma (λ (a) (bind (f a) g)))
|#


#|
assv-ish
'((x . cat) (z . 42) ...)
x
|#

#|
maybe monads : (Maybe A)
- injection or Just: (→ A (Maybe A))
- bind-maybe (→ (Maybe A) (→ A (Maybe B)) (Maybe B))
- Nothing puts something at the effect part (→ (Maybe A))
|#

; (: lookup (→ (Listof (Pairof Symbol Val)) Symbol Val))
; (: lookup (→ (Listof (Pairof Symbol Val)) Symbol (Maybe Val)))
(define lookup
  (λ (ls s)
    (cond
      [(null? ls) (Nothing)]
      [(eqv? (car (car ls)) s) (Just (cdr (car ls)))]
      [else (lookup (cdr ls) s)])))

(define lookup-even?
  (λ (ls s)
    (bind-maybe (lookup ls s)
                (λ (n)
                  (Just (even? n))))))

#;
(lookup-even? '((x . 7) (z . 42)) 'y)
#;
(lookup '((x . cat) (z . 42) (y . not-found)) 'y)
#;
(lookup `((x . ,(Nothing)) (z . 42)) 'x)

#|monad transformers: many different monads at a time|#

#|
(Listof A) is a monad
- bind  (→ (Listof A) (→ A (Listof B)) (Listof B)) is append-map
- inj (→ A (Listof A)) is (λ (a) (cons a '()))
- empty-list (→ (Listof A)) is (λ () '())

- left-id: (append-map `(,a) f) ≡ (f a)
           (append (f a) '())
           (f a)
|#

#|
'((1 2) (3 4 5)) -> '((1 . 3) (1 . 4) (1 . 5) ...)
|#
(define product
  (λ (xs ys)
    (go-on ([x xs]
            [y ys])
      (inj-list `(,x . ,y)))
    #;
    (bind-list xs
               (λ (x)
                 (bind-list ys
                            (λ (y)
                              (inj-list `(,x . ,y))))))))

#;
(product '(1 2) '(3 4 5))

#|
writer monad (log)
- bind (→ (Writer A Log) (→ A (Writer B Log)) (Writer B Log))
- inj  (→ A (Writer A Log))
- tell (→ Log (Writer A Log))
|#
(define fib
  (λ (n)
    (cond
      [(zero? n)
       (go-on ([_ (tell `(fib ,n is 1))])
         (inj-writer 1))]
      [(zero? (sub1 n))
       (go-on ([_ (tell `(fib ,n is 1))])
         (inj-writer 1))]
      [else
       (go-on ([r₁ (fib (sub1 n))]
               [r₂ (fib (sub1 (sub1 n)))]
               [_ (tell `(fib ,n is ,(+ r₁ r₂)))])
         (inj-writer (+ r₁ r₂)))])
    #;
    (cond
      [(zero? n)
       (bind-writer (tell `(fib ,n is 1))
                    (λ (_) (inj-writer 1)))]
      [(zero? (sub1 n))
       (bind-writer (tell `(fib ,n is 1))
                    (λ (_) (inj-writer 1)))]
      [else
       (bind-writer (fib (sub1 n))
                    (λ (r₁)
                      (bind-writer (fib (sub1 (sub1 n)))
                                   (λ (r₂)
                                     (bind-writer (tell `(fib ,n is ,(+ r₁ r₂)))
                                                  (λ (_) (inj-writer (+ r₁ r₂))))))))])))


((run-state (fib/state 5)) '())

(run-writer
 (fib 5))

#|
continuation monad
- inj
- bind
- effect
|#

(define fib/k
  (λ (n)
    (cond
      [(zero? n)
       (inj-k 1)]
      [(zero? (sub1 n))
       (inj-k 1)]
      [else
       (go-on ([r₁ (fib/k (sub1 n))]
               [r₂ (fib/k (sub1 (sub1 n)))])
         (inj-k (+ r₁ r₂)))])))

#;
((run-k
  (fib/k 5))
 (λ (v) v))

; ((run-k
;   (callcc (λ (k) (bind-k (inj-k ())))))
;  (λ (v) v))
