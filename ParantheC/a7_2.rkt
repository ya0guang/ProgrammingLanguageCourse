#lang racket
(require "parenthec.rkt")

; step 1: value-of-cps (expression interpreter) => union case DONE
; step 2: apply-closure => union case DONE

; step 1: add define-union
(define-union expr
    (const cexp)
    (var n)
    (if test conseq alt)
    (mult nexp1 nexp2)
    (sub1 nexp)
    (zero nexp)
    (letcc body)
    (throw kexp vexp)
    (let exp body)              
    (lambda body)
    (app rator rand))

; step 1: change *match* to *union-case*, and remove backquotes and commas.
(define value-of-cps
    (lambda (exp env k)
        (union-case exp expr
            [(const exp) (apply-k k exp)]
            [(mult x1 x2) 
                (value-of-cps x1 env 
                    (make-k-mult1 x2 env k))]
            [(sub1 x) 
                (value-of-cps x env 
                    (make-k-sub1 k))]
            [(zero x) 
                (value-of-cps x env
                    (make-k-zero k))]
            [(if test conseq alt) 
                (value-of-cps test env
                    (make-k-if conseq alt env k))]
            [(letcc body) 
                (value-of-cps body (extend-env k env) k)]
            [(throw k-exp v-exp) 
                (value-of-cps k-exp env 
                    (make-k-throw v-exp env))]
            [(let e body) 
                (value-of-cps e env
                    (make-k-let body env k))]
            [(var y) (apply-env env y k)]
            [(lambda body) 
                (apply-k k (closure_clos body env))]
            [(app rator rand) 
                (value-of-cps rator env
                    (make-k-clos rand env k))])))

(define make-k-clos
    (lambda (rand^ env^ k^)
        `(app-clos ,rand^ ,env^ ,k^)
        #;
        (lambda (v) (value-of-cps rand^ env^
            (make-k-arg v k^)))))

(define make-k-arg
    (lambda (clos^ k^)
        `(app-arg ,clos^ ,k^)
        #;
        (lambda (v) (apply-closure clos^ v k^))))

(define make-k-let
    (lambda (body^ env^ k^)
        `(let ,body^ ,env^ ,k^)
        #;
        (lambda (v) (value-of-cps body^ (extend-env v env^) k^))))

(define make-k-throw
    (lambda (v-exp^ env^)
        `(throw ,v-exp^ ,env^)
        #;
        (lambda (v) (value-of-cps v-exp^ env^ v))))

(define make-k-if
    (lambda (conseq^ alt^ env^ k^)
        `(if ,conseq^ ,alt^ ,env^ ,k^)
        #;
        (lambda (v) (if v (value-of-cps conseq^ env^ k^) (value-of-cps alt^ env^ k^)))))

(define make-k-zero
    (lambda (k^)
        `(zero ,k^)
        #;
        (lambda (v) (apply-k k^ (zero? v)))))

(define make-k-sub1
    (lambda (k^)
        `(sub1 ,k^)
        #;
        (lambda (v) (apply-k k^ (sub1 v)))))

(define make-k-mult1
    (lambda (x2^ env^ k^)
        `(mult1 ,x2^ ,env^ ,k^)
        #;
        (lambda (v) (value-of-cps x2^ env^ 
            (make-k-mult2 v k^)))))

(define make-k-mult2
    (lambda (v1^ k^)
        `(mult2 ,v1^ ,k^)
        #;
        (lambda (v) (apply-k k^ (* v1^ v)))))

(define apply-k
    (lambda (k v)
        (match k
            [`(empty-k) v]
            [`(app-clos ,rand^ ,env^ ,k^) (value-of-cps rand^ env^ (make-k-arg v k^))]
            [`(app-arg ,clos^ ,k^) (apply-closure clos^ v k^)]
            [`(let ,body^ ,env^ ,k^) (value-of-cps body^ (extend-env v env^) k^)]
            [`(throw ,v-exp^ ,env^) (value-of-cps v-exp^ env^ v)]
            [`(if ,conseq^ ,alt^ ,env^ ,k^) (if v (value-of-cps conseq^ env^ k^) (value-of-cps alt^ env^ k^))]
            [`(zero ,k^) (apply-k k^ (zero? v))]
            [`(sub1 ,k^) (apply-k k^ (sub1 v))]
            [`(mult1 ,x2^ ,env^ ,k^) (value-of-cps x2^ env^ (make-k-mult2 v k^))]
            [`(mult2 ,v1^ ,k^) (apply-k k^ (* v1^ v))]
            )))

(define apply-env
    (lambda (env y k^)
        (match env
            [`(empty-env) (error 'value-of-cps "unbound identifier")]
            [`(extend-env ,a^ ,env^) (if (zero? y) (apply-k k^ a^) (apply-env env^ (sub1 y) k^))])))

(define extend-env
    (lambda (a^ env^)
        `(extend-env ,a^ ,env^)
        #;
        (lambda (y k^) (if (zero? y) (apply-k k^ a^) (apply-env env^ (sub1 y) k^)))))

(define empty-env
    (lambda ()
        `(empty-env)
        #;
        (lambda (y k^)
            (error 'value-of-cps "unbound identifier"))))

; removed in step 2
; (define make-closure
;     (lambda (body env)
;         `(clos ,body ,env)
;         #;
;         (lambda (a k) 
;             (value-of-cps body (extend-env a env) k))))

; step 2: define union closure and change the content in apply-closure
; I also change the invocation of *make-closure* into *closure_clos* to ask parentheC to parse it for us
(define-union closure
    (clos body env))

(define apply-closure
    (lambda (clos a k)
        (union-case clos closure
            [(clos body env) (value-of-cps body (extend-env a env) k)])))

(define empty-k
    (lambda ()
        `(empty-k)
        #;
        (lambda (v)
            v)))

; test case yielding 120

(define main 
  (lambda ()
    (value-of-cps 
     (expr_let 
      (expr_lambda
       (expr_lambda 
        (expr_if
         (expr_zero (expr_var 0))
         (expr_const 1)
         (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
      (expr_mult
       (expr_letcc
        (expr_app
         (expr_app (expr_var 1) (expr_var 1))
         (expr_throw (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
       (expr_const 5)))
     (empty-env)
     (empty-k))))

(main)

; obosolete test case
#;
(value-of-cps '(app (if (zero (const 0)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k))