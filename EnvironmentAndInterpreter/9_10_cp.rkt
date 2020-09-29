#lang typed/racket
(require racket/trace)

#|
Representation Independence
- in the last lecture, we used higher-order functions to represent
environments and closures.

- in this lecture, we represent them using data structures (or say, lists).
|#

#|
The 3 equations all take the form:
[ e ]ρ = m

[ y ]ρ       = ρ(y)
[ λ x . b ]ρ = λ a. ([ b ] ρ,x→a)
[ f a ]ρ     = ([f]ρ [a]ρ)

|#

(define-type Exp
  (U Symbol
     Number
     (List 'pluz Exp Exp)
     (List 'λ (List Symbol) Exp)
     (List Exp Exp)))

(define-type Env
  (Listof (Pairof Symbol Meaning)))

(define-type Closure
  (List 'closure Symbol Exp Env))
  
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
       (apply-env env y)]
      [`,n
       #:when (number? n)
       n]
      [`(λ (,x) ,body)
       (make-closure x body env)]
      [`(pluz ,e₁ ,e₂)
       (match (valof e₁ env)
         [`,n₁
          #:when (number? n₁)
          (match (valof e₂ env)
            [`,n₂
             #:when (number? n₂)
             (+ n₁ n₂)]
            [`,whatever
             (error "not a number")])]
         [`,whatever
          (error "not a number")])]
      [`(,rator ,rand)
       (match (valof rator env)
         [`(closure ,x ,body ,env)
          (apply-closure `(closure ,x ,body ,env) (valof rand env))]
         [`,n
          #:when (number? n)
          (error "oops")])])))

(: apply-closure (→ Closure Meaning
                    Meaning))
(define apply-closure
  (λ (clos arg)
    (match clos
      [`(closure ,x ,body ,env)
       (valof body (ext-env x arg env))])
        ))
(: make-closure (→ Symbol Exp Env
                   Closure))
(define make-closure
  (λ (x body env)
    `(closure ,x ,body ,env)))

(: apply-env (→ Env Symbol
                Meaning))
(define apply-env
  (λ (env y)
    (match env
      ['()
       (error "oooooops, unound " y)]
      [`((,x . ,arg) . ,env)
       (if (eqv? y x) arg (apply-env env y))])))

(: ext-env (→ Symbol Meaning Env
              Env))
(define ext-env
  (λ (x arg env)
    `((,x . ,arg) . ,env)))

(: init-env (→ Env))
(define init-env
  (λ ()
    '()))

(valof '(((λ (a) (λ (b) (pluz a b))) 3) 5)
       (init-env))



