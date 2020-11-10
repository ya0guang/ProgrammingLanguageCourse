#lang racket
(require "monads.rkt")

#| Eugenio Moggi, Notions of computation and monads |#
#| Phil Wadler |#
#| Dan Friedman |#

#;
(define fib
  (λ (n)
    (cond
      [(zero? n) 0]
      [(zero? (sub1 n)) 1]
      [else (+ (fib (sub1 n))
               (fib (sub1 (sub1 n))))])))

#| memoization |#
(define fib/table
  (λ (n table)
    (cond
      [(hash-ref table n #f)
       =>
       (λ (result)
         `(,result . ,table))]
      [(zero? n) `(0 . ,table)]
      [(zero? (sub1 n)) `(1 . ,table)]
      [else (match-let* ([`(,fib-sub1 . ,table₁) (fib/table (sub1 n) table)]
                         [`(,fib-sub2 . ,table₂) (fib/table (sub1 (sub1 n)) table₁)]
                         [result (+ fib-sub1 fib-sub2)]
                         [table₃ (hash-set table₂ n result)])
              `(,result . ,table₃))])))

(define table '())
(define fib/effect
  (λ (n)
    (cond
      [(assv n table)
       =>
       (λ (pr)
         (cdr pr))]
      [(zero? n) 0]
      [(zero? (sub1 n)) 1]
      [else (let* ([fib-sub₁ (fib/effect (sub1 n))]
                   [fib-sub₂ (fib/effect (sub1 (sub1 n)))]
                   [result (+ fib-sub₁ fib-sub₂)])
              (begin (set! table `((,n . ,result) . ,table))
                     result))])))

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

#;
((run-state (fib 1000)) '())

#|
state monads  : (State A S)
- injection  (put something at the value part) (→ A (State A S))
- bind (always bind a monads with a function that returns another monad) (→ (State A S) (→ A (State A S)))
- get  (take the effect part)  : (→ (State A S) S)
- put  (put something at the effect part) (→ S (State A S))
|#

#|
 the 3 monads laws
- left-id: (bind (inj a) f) ≡ (f a)
- right-id: (bind ma inj) ≡ ma
- associative: (bind (bind ma f) g) ≡ (bind ma (λ (a) (bind (f a) g)))
|#

#;
((run-state
  (inj-state 5))
 'init)

#;
((run-state
  (bind-state
   (inj-state 5)
   (λ (five) (inj-state (add1 five)))))
 'init)

#;
(bind-state 
 (bind-state
  (inj-state 5)
  (λ (five) (inj-state (add1 five))))
 (λ (_) (put 'new)))

#;
((run-state
  (bind-state
   (bind-state 
    (bind-state
     (inj-state 5)
     (λ (five) (inj-state (add1 five))))
    (λ (_) (put 'new)))
   (λ (_) (inj-state 6))))
 'init)


#;
'(_ (6 . init))
