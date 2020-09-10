#lang typed/racket

#|
Free and bound variables of λ-calculus
- a variable occurs free, if no corresponding binder wraps around it
  E.g., the variable y occurs free in (λ (x) (x y))
- a variable occurs bounds, if a corresponding binder wraps around it
  E.g., the variable x occurs bound in (λ (x) (x y))
|#

#;
(λ (a)
  (λ (b)
    (λ (a)
      (λ (d)
        ((a b) (c d))))))

#;
(λ (a) (λ (a) a))


#|
An expression (of λ-calculus) is any one of the following.
- a variable, x 
- an abstraction, (λ (x) body), if body is an expression
- an application, (rator rand), if rator and rand are expressions
|#
(define-type Exp
  (U Symbol
     (List 'λ (List Symbol) Exp)
     (List Exp Exp)))

(: free? (→ Symbol Exp
            Boolean))
(define free?
  (λ (s e)
    (match e
      [`,y
       #:when (symbol? y)
       (eqv? y s)]
      [`(λ (,x) ,body)
       (and (not (eqv? x s))
            (free? s body))]
      [`(,rator ,rand)
       (or (free? s rator)
           (free? s rand))])))

#;
(free? 'y '(λ (x) y))
#;
(free? 'x '(λ (x) y))
#;
(free? 'a '(λ (a) (a b y)))

(: bound? (→ Symbol Exp
             Boolean))
(define bound?
  (λ (s e)
    (match e
      [`,y
       #:when (symbol? y)
       #f]
      [`(λ (,x) ,body)
       (or (and (eqv? x s) (free? s body))
           (bound? s body))]
      [`(,rator ,rand)
       (or (bound? s rator)
           (bound? s rand))])))

#;
(bound? 'a '(λ (a) (λ (a) (a b))))
