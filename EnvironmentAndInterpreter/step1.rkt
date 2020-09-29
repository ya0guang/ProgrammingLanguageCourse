#lang typed/racket/no-check
(require racket/trace)

#|
Representation Independence
- in the last lecture, we used higher-order functions to represent
  environments and closures.

- in this lecture, we represent them using data structures (or say, lists).
|#

(define-type Exp
  (U Symbol
     (List 'pluz Exp Exp)
     (List 'λ (List Symbol) Exp)
     (List Exp Exp)))

(define-type Env
  (→ Symbol Meaning))
(define-type Closure
  (→ Meaning Meaning))
(define-type Meaning
  (U Number
     Closure))

(: valof (→ Exp Env
            Meaning))
(define valof
  (λ (exp env)
    (match exp
      [`,y
       #:when (symbol? y)
       (env y)]
      [`,n
       #:when (number? n)
       n]
      [`(λ (,x) ,body)
       (λ (arg) (valof body (λ (y) (if (eqv? y x) arg (env y)))))]
      [`(pluz ,e₁ ,e₂)
       (+ (valof e₁ env) (valof e₂ env))]
      [`(,rator ,rand)
       ((valof rator env) (valof rand env))])))

(: init-env (→ Env))
(define init-env
  (λ ()
    (λ (y) (error "oops, unound " y))))

(trace valof)
(valof '(((λ (a) (λ (b) (pluz a b))) 3) 5)
       (init-env))


