#lang typed/racket/no-check

(define-type Exp
  (U Symbol
     (List 'pluz Exp Exp)
     (List 'λ (List Symbol) Exp)
     (List Exp Exp)))

#;
(((λ (add1)
     (λ (zero)
       (add1 (add1 (add1 zero)))))
  (λ (n) (cons '() n)))
 '())

#|
- If the expression is a variable, then look it up in the environment.
- If the expression is a λ-abstraction, make a closure.
  A closure wraps up the environment, the body of the abstraction, and the binding variable of the abstraction.
- If the expression is an application, get the values of the rator and the rand, then apply them.
|#

(define-type Env
  (→ Symbol Meaning))
(define-type Closure
  (→ Meaning Meaning))
(define-type Meaning
  (U Number
     Closure))

(: valof (→ Exp Env
            Meaning))
#;
(define valof
  (λ (exp env)
    (match exp
      [`,y
       #:when (symbol? y)
       (env y)]
      [`(λ (,x) ,body)
       (λ (arg) (valof body (λ (y) (if (eqv? y x) arg (env y)))))]
      [`(,rator ,rand)
       ((valof rator env) (valof rand env))])))
(define valof
  (λ (exp env)
    (match exp
      [`,y
       #:when (symbol? y)
       (env y)]
      [`(λ (,x) ,body)
       (λ ([arg : Meaning]) (valof body (λ (y) (if (eqv? y x) arg (env y)))))]
      [`(pluz ,e₁ ,e₂)
       (match (valof e₁ env)
         [`,n₁
          #:when (number? n₁)
          (match (valof e₂ env)
            [`,n₂
             #:when (number? n₂)
             (+ n₁ n₂)]
            [`,whatever
             (error "oops")])]
         [`,whatever
          (error "oops")])]
      [`(,rator ,rand)
       (match (valof rator env)
         [`,f
          #:when (procedure? f)
          (f (valof rand env))]
         [`,whatever
          (error "oops")])])))

#|
(λ (arg) (valof body (λ (y) (if (eqv? y x) arg (env y)))))
x body     env
a (+ a b) (b -> 5)
(λ (a) (+ a b))
|#

(define ROMAN-ENV
  (λ (y)
    (match y
      [`I 1]
      [`V 5]
      [`X 10]
      [`L 50]
      [`C 100]
      [`D 500]
      [`M 1000])))

#;
(valof
 '(((λ (b) (λ (a) (pluz b a))) I) X)
 ROMAN-ENV)
#;
(valof
 '((((λ (a) (λ (b) (λ (a) (pluz a b)))) X)
    L)
   V)
 ROMAN-ENV)

