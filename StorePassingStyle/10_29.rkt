#lang racket

(define fib
  (λ (n)
    (cond
      [(zero? n) 0]
      [(zero? (sub1 n)) 1]
      [else (+ (fib (sub1 n))
               (fib (sub1 (sub1 n))))])))

#;
(fib 50)

(define fib/counter
  (λ (n counter)
    (cond
      [(zero? n) `(0 . ,counter)]
      [(zero? (sub1 n)) `(1 . ,counter)]
      [else (match-let* ([`(,fib-sub1 . ,counter1) (fib/counter (sub1 n) (add1 counter))]
                         [`(,fib-sub2 . ,counter2) (fib/counter (sub1 (sub1 n)) (add1 counter1))])
              `(,(+ fib-sub1 fib-sub2) . ,counter2))])))

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


(define valof
  (λ (exp env)
    (match exp
      [`,y
       #:when (symbol? y)
       (env y)]
      [`,n
       #:when (number? n)
       n]
      [`(if ,test ,then ,else)
       (if (valof test env)
           (valof then env)
           (valof else env))]
      [`(zero? ,e)
       (zero? (valof e env))]
      [`(sub1 ,e)
       (sub1 (valof e env))]
      [`(* ,e₁ ,e₂)
       (* (valof e₁ env) (valof e₂ env))]
      [`(λ (,x) ,body)
       (λ (a) (valof body (λ (y) (if (eqv? y x) a (env y)))))]
      [`(,rator ,rand)
       ((valof rator env) (valof rand env))])))

(define valof/log
  (λ (exp env log)
    (match exp
      [`,y
       #:when (symbol? y)
       (let ([log `(,y . ,log)])
         `(,(env y) . ,log))]
      [`,n
       #:when (number? n)
       (let ([log `(,n . ,log)])
         `(,n . ,log))]
      [`,b
       #:when (boolean? b)
       (let ([log `(,b . ,log)])
         `(,b . ,log))]
      [`(if ,test ,then ,else)
       (match-let* ([log₁ `((if ,test ,then ,else) . ,log)]
                    [`(,b . ,log₂) (valof/log test env log₁)])
         (if b
             (valof/log then env log₂)
             (valof/log else env log₂)))]
      [`(zero? ,e)
       (match-let* ([log₁ `((zero? ,e) . ,log)]
                    [`(,r . ,log₂) (valof/log e env log₁)])
         `(,(zero? r) . ,log₂))]
      [`(sub1 ,e)
       (match-let* ([log₁ `((sub1 ,e) . ,log)]
                    [`(,r . ,log₂) (valof/log e env log₁)])
         `(,(sub1 r) . ,log₂))]
      [`(* ,e₁ ,e₂)
       (match-let* ([log₁ `((* ,e₁ ,e₂) . ,log)]
                    [`(,v₁ . ,log₂) (valof/log e₁ env log₁)]
                    [`(,v₂ . ,log₃) (valof/log e₂ env log₂)])
         `(,(*  v₁ v₂) . ,log₃))]
      [`(λ (,x) ,body)
       (let ([log₀ `((λ (,x) ,body) . ,log)])
         `(,(λ (a log) (valof/log body (λ (y) (if (eqv? y x) a (env y))) log)) . ,log₀))]
      [`(,rator ,rand)
       (match-let* ([log₁ `((,rator ,rand) . ,log)]
                    [`(,clos . ,log₂) (valof/log rator env log₁)]
                    [`(,arg . ,log₃) (valof/log rand env log₂)])
         (clos arg log₃))])))

(valof/log '(((λ (x) (x x))
              (λ (fact)
                (λ (n)
                  (if (zero? n)
                      1
                      (* n ((fact fact) (sub1 n)))))))
             5)
           (λ (y) (error "oops"))
           '())
