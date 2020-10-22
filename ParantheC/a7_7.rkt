#lang racket
(require "parenthec.rkt")
(require racket/trace)

; step 1: value-of-cps (expression interpreter) => union case DONE
; step 2: apply-closure => union case DONE
; step 3: apply-env => union case (environment constructor) DONE
; step 4: apply-k => union case (continuation constructor) DONE
; step 5: transform serious calls to a-normal form Almostly DONE
; step 6: registerization DONE
; step 7: define-label and program counter

; step 6 notes
; from the simplest one to the most complex one: apply-closure, apply-env, apply-k, value-of-cps
; 1. rewrite the function to register form
; 2. rewrite every call to the function: use begin and register to pass arguments
; 3. test!
; 4. repeat 1 and 2 on the next funtion to be registerized
; 5. rewrite main: reconstruct main in "begin-set" form

; step 6: register definations
(define-registers
    exp k-cps ;env-cps for value-of-cps
    v;k k^ v ; for apply-k
    clos a-ac ;k-ac for apply-closure
    env y-ae ;k-ae for apply-env
)

; step 7
(define-program-counter pc)

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
(define-label value-of-cps
    (union-case exp expr
        [(const cexp) 
            (begin
                ;(set! k k-cps)
                (set! v cexp)
                (apply-k))]
        [(mult x1 x2) 
            (begin
                (set! k-cps (continuation_mult1 x2 env k-cps))
                (set! exp x1)
                (value-of-cps))]
        [(sub1 x)
            (begin
                (set! k-cps (continuation_sub1 k-cps))
                (set! exp x)
                (value-of-cps))]
        [(zero x) 
            (begin
                (set! k-cps (continuation_zero k-cps))
                (set! exp x) 
                (value-of-cps))]
        [(if test conseq alt) 
            (begin
                (set! k-cps (continuation_if conseq alt env k-cps))
                (set! exp test)
                (value-of-cps))]
        [(letcc body) 
            (begin 
                ;Q: originally env-cps here
                (set! env (environment_extend-env k-cps env))
                (set! exp body)
                (value-of-cps))]
        [(throw k-exp v-exp)
            (begin
                (set! k-cps (continuation_throw v-exp env))
                (set! exp k-exp)
                (value-of-cps))]
        [(let e body) 
            (begin
                (set! k-cps (continuation_let body env k-cps))
                (set! exp e)
                (value-of-cps))]
        [(var y) 
            (begin
                (set! y-ae y)
                ;(set! k-ac k-cps)
                (apply-env))
            #;
            (apply-env env-cps y k-cps)]
        [(lambda body)
            (begin 
                (set! v (closure_clos body env))
                ;(set! k k-cps)
                (apply-k))]
        [(app rator rand)
            (begin
                (set! k-cps (continuation_app-clos rand env k-cps))
                (set! exp rator) 
                (value-of-cps))]))

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

(define-label apply-k
    (union-case k-cps continuation
        [(empty-k) v]
        [(app-clos rand^ env^ k^)
            (begin 
                (set! k-cps (continuation_app-arg v k^))
                (set! exp rand^)
                (set! env env^)
                (value-of-cps))]
        [(app-arg clos^ k^)
            (begin 
                (set! clos clos^)
                (set! a-ac v)
                (set! k-cps k^)
                (apply-closure))
            #;
            (apply-closure clos^ v k^)]
        [(let body^ env^ k^) 
            (begin
                (set! env (environment_extend-env v env^))
                (set! exp body^)
                (set! k-cps k^)
                (value-of-cps))]
        [(throw v-exp^ env^) 
            (begin
                (set! exp v-exp^)
                (set! env env^)
                (set! k-cps v)
                (value-of-cps))]
        [(if conseq^ alt^ env^ k^) 
            (if v 
                (begin
                    (set! exp conseq^)
                    (set! env env^)
                    (set! k-cps k^)
                    (value-of-cps)) 
                (begin
                    (set! exp alt^)
                    (set! env env^)
                    (set! k-cps k^)
                    (value-of-cps)))]
        [(zero k^) 
            ;Q: v or v^?
            (begin
                (set! k-cps k^)
                (set! v (zero? v))
                (apply-k))]
        [(sub1 k^) 
            (begin 
                (set! v (sub1 v))
                (set! k-cps k^)
                (apply-k))]
        [(mult1 x2^ env^ k^) 
            (begin 
                (set! k-cps (continuation_mult2 v k^))
                (set! exp x2^)
                (set! env env^)
                (value-of-cps))]
        [(mult2 v1^ k^) 
            (begin
                (set! v (* v1^ v))
                (set! k-cps k^)
                (apply-k))]
        ))

; step 3: define-union for environment 
(define-union environment
    (empty-env)
    (extend-env a^ env^))

; step 3: change to union-case
(define-label apply-env
    (union-case env environment
        [(empty-env) (error 'value-of-cps "unbound identifier")]
        [(extend-env a^ env^)
            ; Q: do I need to a-normal here?
            (if (zero? y-ae) 
                (begin 
                    (set! v a^)
                    ;(set! k-ac k) ; Q: may not need
                    (apply-k)
                    #;(apply-k k-ac a^)
                    )

                (begin
                    (set! env env^)
                    (set! y-ae (sub1 y-ae))
                    (apply-env))
                )]))

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

(define-label apply-closure
    (union-case clos closure
        ; change env to env^ to avoid conflict of register and local var
        [(clos body env^) 
            (begin
                (set! env (environment_extend-env a-ac env^))
                (set! exp body)
                ;(set! k-cps k-ac)
                (value-of-cps))]))

; (define empty-k
;     (lambda ()
;         `(empty-k)
;         #;
;         (lambda (v)
;             v)))
(define-label main
    (begin 
        (set! exp      
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
            (expr_const 5))))
        (set! env (environment_empty-env))
        (set! k-cps (continuation_empty-k))
        (value-of-cps)))

(main)



; test case yielding 120
; (define main 
;   (lambda ()
;     (value-of-cps 
;      (expr_let 
;       (expr_lambda
;        (expr_lambda 
;         (expr_if
;          (expr_zero (expr_var 0))
;          (expr_const 1)
;          (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
;       (expr_mult
;        (expr_letcc
;         (expr_app
;          (expr_app (expr_var 1) (expr_var 1))
;          (expr_throw (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
;        (expr_const 5)))
;      ; invocate environment_empty-env in step 3
     
;      ; invocate continuation_empty-k in step 4
;      (continuation_empty-k))))

; (main)

; obosolete test case
; (value-of-cps '(app (if (zero (const 0)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k))