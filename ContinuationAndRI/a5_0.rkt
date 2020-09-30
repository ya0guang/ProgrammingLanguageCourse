#lang racket
(require racket/trace)

; 1 last-non-zero
(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
	((last-non-zero
	   (lambda (ls)
	     (cond
	       ((null? ls) ls)
               ((zero? (car ls)) (k (last-non-zero (cdr ls))))
  	       (else (cons (car ls) (last-non-zero (cdr ls))))))))
	(last-non-zero ls)))))

; (trace last-non-zero)
; (last-non-zero '(0))
; (last-non-zero '(1 2 3 0 4 5))
; (last-non-zero '(0 4 5))
; (last-non-zero '(1 0 2 3 0 4 5))
; (last-non-zero '(1 2 3 4 5))

; 2 lex

(define (apply-env-lex env num)
    (cond
        ((zero? num) (car env))
        (else (apply-env-lex (cdr env) (sub1 num)))))

(define (extend-env-lex m env)
    (cons m env))

(define empty-env-lex 
    (lambda () '()))


; (define value-of-lex
;     (lambda (exp env)
;         (match exp
;             [`(zero? ,e)
;                 (zero? (value-of-lex e env))]
;             [`(const ,n)
;                 n]
;             [`(sub1 ,e)
;                 (sub1 (value-of-lex e env))]
;             [`(mult ,e1 ,e2)
;                 (* (value-of-lex e1 env) (value-of-lex e2 env))]
;             [`(if ,test ,et ,el)
;                 (if (value-of-lex test env) (value-of-lex et env) (value-of-lex el env))]
;             [`(letcc ,body)
;                 (let/cc k
;                     (value-of-lex body (extend-env-lex k env)))]
;             [`(throw ,k ,v)
;                 ((value-of-lex k env) (value-of-lex v env))]
;             [`(var ,num)
;                 #:when (number? num)
;                 (apply-env-lex env num)]
;             [`(lambda ,body) 
;                 (lambda (a) (value-of-lex body (extend-env-lex a env)))]
;             [`(,rator ,rand)
;                 ((value-of-lex rator env) (value-of-lex rand env))])))

(define value-of-lex
    (lambda (exp env)
        (match exp
            [`(zero? ,e)
                `(zero ,(value-of-lex e env))]
            [`(const ,n)
                n]
            [`(sub1 ,e)
                (sub1 (value-of-lex e env))]
            [`(mult ,e1 ,e2)
                `(mult ,(value-of-lex e1 env) ,(value-of-lex e2 env))]
            [`(if ,test ,et ,el)
                (if (value-of-lex test env) (value-of-lex et env) (value-of-lex el env))]
            [`(letcc ,body)
                (let/cc k
                    (value-of-lex body (extend-env-lex k env)))]
            [`(throw ,k ,v)
                ((value-of-lex k env) (value-of-lex v env))]
            [`(var ,num)
                #:when (number? num)
                (apply-env-lex env num)]
            [`(lambda ,body) 
                (lambda (a) (value-of-lex body (extend-env-lex a env)))]
            [`(,rator ,rand)
                `(app ,(value-of-lex rator env) ,(value-of-lex rand env))])))

; test for original value-of-lex
; (value-of-lex '((lambda (var 0)) (const 5)) (empty-env-lex))
; (value-of-lex '((lambda ((lambda (mult (var 0) (var 1))) (const 4))) (const 5)) (empty-env-lex))
; (value-of-lex '((lambda (zero? (sub1 (var 0)))) (const 1)) (empty-env-lex))
; (value-of-lex '((lambda (if (zero? (var 0)) (const 1) (const 2))) (const 0)) (empty-env-lex))
; (value-of-lex '((lambda (if (zero? (var 0)) (const 1) (const 2))) (const 1)) (empty-env-lex))
; (value-of-lex '(letcc (((var 0) (const 11)) ((var 0) (const 22)))) (empty-env-lex))
; (value-of-lex '(throw (lambda (var 0)) (const 5)) (empty-env-lex))

; 3 The interpreter

(define value-of
    (lambda (expr env)
        (match expr
            [`(const ,expr) expr]
            [`(mult ,x1 ,x2) (* (value-of x1 env) (value-of x2 env))]
            [`(sub1 ,x) (sub1 (value-of x env))]
            [`(zero ,x) (zero? (value-of x env))]
            [`(if ,test ,conseq ,alt) (if (value-of test env)
                                            (value-of conseq env)
                                            (value-of alt env))]
            [`(letcc ,body) (let/cc k
                                (value-of body (lambda (y) (if (zero? y) k (env (sub1 y))))))]
            [`(throw ,k-exp ,v-exp) ((value-of k-exp env) (value-of v-exp env))]
            [`(let ,e ,body) (let ((a (value-of e env)))
                                (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
            [`(var ,y) (env y)]
            [`(lambda ,body) (lambda (a) (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
            [`(app ,rator ,rand) ((value-of rator env) (value-of rand env))])))
    
(define empty-env
    (lambda ()
        (lambda (y)
            (error 'value-of "unbound identifier"))))
    
(define empty-k
    (lambda ()
        (lambda (v)
            v)))


(value-of '(app (lambda (var 0)) (const 5)) (empty-env))
; (value-of '((lambda ((lambda (mult (var 0) (var 1))) (const 4))) (const 5)) (empty-env))
; (value-of '((lambda (zero (sub1 (var 0)))) (const 1)) (empty-env))
; (value-of '((lambda (if (zero (var 0)) (const 1) (const 2))) (const 0)) (empty-env))
; (value-of '((lambda (if (zero (var 0)) (const 1) (const 2))) (const 1)) (empty-env))
; (value-of '(letcc (((var 0) (const 11)) ((var 0) (const 22)))) (empty-env))
; (value-of '(throw (lambda (var 0)) (const 5)) (empty-env))