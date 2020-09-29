#lang racket

#;
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
      [`(let/cc ,cc ,body)
       (let/cc k (valof body (λ (y) (if (eqv? y cc) k (env y)))))]
      [`(throw ,k ,e)
       ((valof k env) (valof e env))]
      [`(pluz ,e₁ ,e₂)
       (+ (valof e₁ env) (valof e₂ env))]
      [`(,rator ,rand)
       ((valof rator env) (valof rand env))])))

(define valof-cps
  (λ (exp env-cps k)
    (match exp
      [`,y
       #:when (symbol? y)
       (env-cps y k)]
      [`,n
       #:when (number? n)
       (k n)]
      [`(λ (,x) ,body)
       (k (λ (arg k) (valof-cps body (λ (y k^) (if (eqv? y x) (k^ arg) (env-cps y k^))) k)))]
      [`(let/cc ,cc ,body)
       (valof-cps body (λ (y k^) (if (eqv? y cc) (k^ k) (env-cps y k^))) k)]
      [`(throw ,k ,e)
       (valof-cps k env-cps
                  (λ (k)
                    (valof-cps e env-cps k)))]
      [`(pluz ,e₁ ,e₂)
       (valof-cps e₁ env-cps
                  (λ (n₁)
                    (valof-cps e₂ env-cps
                               (λ (n₂)
                                 (k (+ n₁ n₂))))))]
      [`(,rator ,rand)
       (valof-cps rator env-cps
                  (λ (clos)
                    (valof-cps rand env-cps
                               (λ (arg)
                                 (clos arg k)))))])))

#;
(valof '(((λ (a) (λ (b) (pluz a b))) 5) 7)
       (λ (y) (error "oops")))

(let ([r (let/cc k (k (let/cc k (+ (k (k k)) (k 5)))))])
  r)

#;
(valof-cps '(((λ (a) (λ (b) (pluz a b))) 6) 5)
           (λ (y k) (error "oops"))
           (λ (v) v))
#;
(valof-cps '(let/cc k (((λ (a) (λ (b) (pluz (throw k a) b))) 6) 5))
           (λ (y k) (error "oops"))
           (λ (v) v))

#|
(λ (v) (add1 v)) ≡ add1

η-reduction:
(λ (x) (f x)) ≡ f,
if 1, x does not occur free in f
2, f is not an infinite loop

|#

