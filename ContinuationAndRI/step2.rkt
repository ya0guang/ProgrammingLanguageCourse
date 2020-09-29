#lang racket

(define valof-cps
  (λ (exp env-cps k)
    (match exp
      [`,y
       #:when (symbol? y)
       (env-cps y k)]
      [`,n
       #:when (number? n)
       (apply-k k n)]
      [`(λ (,x) ,body)
       (apply-k k (λ (arg k) (valof-cps body (λ (y k^) (if (eqv? y x) (k^ arg) (env-cps y k^))) k)))]
      [`(let/cc ,cc ,body)
       (valof-cps body (λ (y k^) (if (eqv? y cc) (k^ k) (env-cps y k^))) k)]
      [`(throw ,k ,e)
       (valof-cps k env-cps (make-k-throw e env-cps))]
      [`(pluz ,e₁ ,e₂)
       (valof-cps e₁ env-cps (make-k-pluz₁ e₂ env-cps k))]
      [`(,rator ,rand)
       (valof-cps rator env-cps (make-k-clos rand env-cps k))])))

(define apply-k
  (λ (k v)
    (k v)))

(define make-k-pluz₁
  (λ (e₂ env-cps k)
    (λ (v)
      (valof-cps e₂ env-cps
                 (make-k-pluz₂ v k)))))

(define make-k-pluz₂
  (λ (n₁ k)
    (λ (v)
      (k (+ n₁ v)))))

(define make-k-throw
  (λ (e env-cps)
    (λ (v)
      (valof-cps e env-cps v))))

(define make-k-clos
  (λ (rand env-cps k)
    (λ (v)
      (valof-cps rand env-cps
                 (make-k-arg v k)))))

(define make-k-arg 
  (λ (clos k)
    (λ (v)
      (clos v k))))

(define make-k-init
  (λ ()
    (λ (v) v)))


(valof-cps '((λ (x) x) (let/cc k (((λ (a) (λ (b) (pluz (throw k a) b))) 6) 5)))
           (λ (y k) (error "oops"))
           (λ (v) v))
