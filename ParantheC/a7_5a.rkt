#lang racket
(require "parenthec.rkt")

; step 1: value-of-cps (expression interpreter) => union case DONE
; step 2: apply-closure => union case DONE
; step 3: apply-env => union case (environment constructor) DONE
; step 4: apply-k => union case (continuation constructor) DONE
; step 5: transform serious calls to a-normal form Almostly DONE

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
; before step 5: rename the paramters env => env-cps, k => k-cps
(define value-of-cps
    (lambda (exp env-cps k-cps)
        (union-case exp expr
            [(const exp) (apply-k k-cps exp)]
            [(mult x1 x2) 
                (let* ([k-cps (continuation_mult1 x2 env-cps k-cps)])
                    (value-of-cps x1 env-cps k-cps))]
            [(sub1 x)
                (let* ([k-cps (continuation_sub1 k-cps)])
                    (value-of-cps x env-cps k-cps))]
            [(zero x) 
                (let* ([k-cps (continuation_zero k-cps)]) 
                    (value-of-cps x env-cps k-cps))]
            [(if test conseq alt) 
                (let* ([k-cps (continuation_if conseq alt env-cps k-cps)])
                    (value-of-cps test env-cps k-cps))]
            [(letcc body) 
                (let* ([env-cps (environment_extend-env k-cps env-cps)])
                    (value-of-cps body env-cps k-cps))]
            [(throw k-exp v-exp)
                (let* ([k-cps (continuation_throw v-exp env-cps)])
                    (value-of-cps k-exp env-cps k-cps))]
            [(let e body) 
                (let* ([k-cps (continuation_let body env-cps k-cps)])
                    (value-of-cps e env-cps k-cps))]
            [(var y) (apply-env env-cps y k-cps)]
            [(lambda body)
                (let* ([v (closure_clos body env-cps)])
                    (apply-k k-cps v))]
            [(app rator rand)
                (let* ([k-cps (continuation_app-clos rand env-cps k-cps)]) 
                    (value-of-cps rator env-cps k-cps))])))

; removed in step 4

; (define make-k-clos
;     (lambda (rand^ env^ k^)
;         `(app-clos ,rand^ ,env^ ,k^)
;         #;
;         (lambda (v) (value-of-cps rand^ env^
;             (make-k-arg v k^)))))

; (define make-k-arg
;     (lambda (clos^ k^)
;         `(app-arg ,clos^ ,k^)
;         #;
;         (lambda (v) (apply-closure clos^ v k^))))

; (define make-k-let
;     (lambda (body^ env^ k^)
;         `(let ,body^ ,env^ ,k^)
;         #;
;         (lambda (v) (value-of-cps body^ (extend-env v env^) k^))))

; (define make-k-throw
;     (lambda (v-exp^ env^)
;         `(throw ,v-exp^ ,env^)
;         #;
;         (lambda (v) (value-of-cps v-exp^ env^ v))))

; (define make-k-if
;     (lambda (conseq^ alt^ env^ k^)
;         `(if ,conseq^ ,alt^ ,env^ ,k^)
;         #;
;         (lambda (v) (if v (value-of-cps conseq^ env^ k^) (value-of-cps alt^ env^ k^)))))

; (define make-k-zero
;     (lambda (k^)
;         `(zero ,k^)
;         #;
;         (lambda (v) (apply-k k^ (zero? v)))))

; (define make-k-sub1
;     (lambda (k^)
;         `(sub1 ,k^)
;         #;
;         (lambda (v) (apply-k k^ (sub1 v)))))

; (define make-k-mult1
;     (lambda (x2^ env^ k^)
;         `(mult1 ,x2^ ,env^ ,k^)
;         #;
;         (lambda (v) (value-of-cps x2^ env^ 
;             (make-k-mult2 v k^)))))

; (define make-k-mult2
;     (lambda (v1^ k^)
;         `(mult2 ,v1^ ,k^)
;         #;
;         (lambda (v) (apply-k k^ (* v1^ v)))))

; step 4: define union for continuations

(define-union continuation
    (empty-k)
    (app-clos rand^ env^ k^)
    (app-arg clos^ k^)    
    (let body^ env^ k^)
    (throw v-exp^ env^)
    (if conseq^ alt^ env^ k^)
    (zero k^)
    (sub1 k^)
    (mult1 x2^ env^ k^)
    (mult2 v1^ k^))

(define apply-k
    (lambda (k v)
        (union-case k continuation
            [(empty-k) v]
            [(app-clos rand^ env^ k^)
                (let* ([k^ (continuation_app-arg v k^)])
                    (value-of-cps rand^ env^ k^))]
            [(app-arg clos^ k^) (apply-closure clos^ v k^)]
            [(let body^ env^ k^) 
                (let* ([env^ (environment_extend-env v env^)])
                    (value-of-cps body^ env^ k^))]
            [(throw v-exp^ env^) (value-of-cps v-exp^ env^ v)]
            [(if conseq^ alt^ env^ k^) (if v (value-of-cps conseq^ env^ k^) (value-of-cps alt^ env^ k^))]
            [(zero k^) 
                ;Q: v or v^?
                (let* ([v (zero? v)])
                    (apply-k k^ v))]
            [(sub1 k^) 
                (let* ([v (sub1 v)])
                    (apply-k k^ v))]
            [(mult1 x2^ env^ k^) 
                (let* ([k^ (continuation_mult2 v k^)])
                    (value-of-cps x2^ env^ k^))]
            [(mult2 v1^ k^) 
                (let* ([v (* v1^ v)])
                    (apply-k k^ v))]
            )))

; step 3: define-union for environment 
(define-union environment
    (empty-env)
    (extend-env a^ env^))

; step 3: change to union-case
(define apply-env
    (lambda (env y k^)
        (union-case env environment
            [(empty-env) (error 'value-of-cps "unbound identifier")]
            [(extend-env a^ env^)
                ; Q: do I need to a-normal here?
                (if (zero? y) (apply-k k^ a^) (apply-env env^ (sub1 y) k^))])))

; removed in step 3
; (define extend-env
;     (lambda (a^ env^)
;         `(extend-env ,a^ ,env^)
;         #;
;         (lambda (y k^) (if (zero? y) (apply-k k^ a^) (apply-env env^ (sub1 y) k^)))))

; (define empty-env
;     (lambda ()
;         `(empty-env)
;         #;
;         (lambda (y k^)
;             (error 'value-of-cps "unbound identifier"))))

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
            [(clos body env) 
                (let* ([env (environment_extend-env a env)])
                    (value-of-cps body env k))])))

; (define empty-k
;     (lambda ()
;         `(empty-k)
;         #;
;         (lambda (v)
;             v)))

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
     ; invocate environment_empty-env in step 3
     (environment_empty-env)
     ; invocate continuation_empty-k in step 4
     (continuation_empty-k))))

(main)

; obosolete test case
#;
(value-of-cps '(app (if (zero (const 0)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k))