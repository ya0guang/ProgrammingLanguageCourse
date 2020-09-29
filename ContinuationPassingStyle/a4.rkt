#lang typed/racket/no-check

(require racket/trace)

(define-type Countinuation
    (-> Number Number))

; empty continuation
; (: empty-k (-> Countinuation))
; (define empty-k
;     (lambda ()
;         (lambda (v) v)))


(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))

; 1 binary-to-decimal

(: binary-to-decimal (-> (Listof Number) Number))
(define binary-to-decimal
    (lambda (n)
        (cond
            [(null? n) 0]
            [else (+ (car n) (* 2 (binary-to-decimal (cdr n))))])))


(: binary-to-decimal-cps (-> (Listof Number) Countinuation Number))
(define binary-to-decimal-cps
  (lambda (n k)
    (cond
        [(null? n) (k 0)]
        [else (binary-to-decimal-cps (cdr n) 
            (lambda (v) (k (+ (* 2 v) (car n)))))])))

; (trace binary-to-decimal-cps)
; (binary-to-decimal-cps '(1 1 0 1) (empty-k))

; 2 times-cps
(: times (-> (Listof Number) Number))
(define times
    (lambda (ls)
        (cond
            [(null? ls) 1]
            [(zero? (car ls)) 0]
            [else (* (car ls) (times (cdr ls)))])))

(: times-cps (-> (Listof Number) Countinuation Number))
(define times-cps
    (lambda (ls k)
        (cond
        [(null? ls) (k 1)]
        ;   [(zero? (car ls)) 0]
        [else (times-cps (cdr ls)
            (lambda (v) (k (* (car ls) v))))])))

; test
; (times-cps '(1 2 3 0 3) (empty-k))
; (times-cps '(1 2 3 4 5) (empty-k))

; 3 times-cps-shortcut
(: times-cps-shortcut (-> (Listof Number) Countinuation Number))
(define times-cps-shortcut
    (lambda (ls k)
        (cond
            [(null? ls) (k 1)]
            [(zero? (car ls)) 0]
            [else (times-cps-shortcut (cdr ls)
                (lambda (v) (k (* (car ls) v))))])))

; 4 plus-cps
(: plus (-> Number (-> Number Number)))
(define plus
    (lambda (m)
        (lambda (n)
            (+ m n))))

; (: plus-cps (→ Number (→ (→ Number (→ Number Number) Number) (→ Number (→ Number Number) Number))
;                (→ Number (→ Number Number) Number)))
(: plus-cps (-> Number (-> (-> Number Countinuation Number) (-> Number Countinuation Number)) (-> Number Countinuation Number)))
(define plus-cps
    (lambda (m km)
        (km (lambda (n kn)
            (kn (+ m n))))))

; test
; (plus-cps 3 (lambda (f) (f 5 (empty-k))))

; 5 remv-first-9*-cps
(define-type List-5
    (Listof (U Number Null List-5)))

(: remv-first-9* (-> List-5 List-5))
(define remv-first-9*
    (lambda (ls)
        (cond
            [(null? ls) '()]
            [(pair? (car ls))
                (cond
                    [(equal? (car ls) (remv-first-9* (car ls)))
                        (cons (car ls) (remv-first-9* (cdr ls)))]
                    [else (cons (remv-first-9* (car ls)) (cdr ls))])]
            [(eqv? (car ls) '9) (cdr ls)]
            [else (cons (car ls) (remv-first-9* (cdr ls)))])))


(: remv-first-9*-cps (-> List-5 (-> List-5 List-5) List-5))
(define remv-first-9*-cps
    (lambda (ls k)
        (cond
            [(null? ls) (k ls)]
            [(pair? (car ls))
                (remv-first-9*-cps (car ls) 
                    (lambda (v1) 
                        (cond
                            [(equal? (car ls) v1)
                                (remv-first-9*-cps (cdr ls) 
                                    (lambda (v2) (k (cons v1 v2))))]
                            [else (remv-first-9*-cps (car ls)
                                (lambda (v2) (k (cons v2 (cdr ls)))))])))]
            [(eqv? (car ls) '9) (k (cdr ls))]
            [else (remv-first-9*-cps (cdr ls) 
                (lambda (v) (k (cons (car ls) v))))])))

; test
; (remv-first-9*-cps '((1 2 (3) 9)) (empty-k))
; (remv-first-9*-cps '(9 (9 (9 (9)))) (empty-k))
; (remv-first-9*-cps '(((((9) 9) 9) 9) 9) (empty-k))

; 6 cons-cell-count-cps
(: cons-cell-count (-> Any Number))
(define cons-cell-count
    (lambda (ls)
        (cond
            [(pair? ls)
                (add1 (+ (cons-cell-count (car ls)) (cons-cell-count (cdr ls))))]
            [else 0])))


(: cons-cell-count-cps (-> Any (-> Any Any) Number))
(define cons-cell-count-cps
    (lambda (ls k)
        (cond
            [(pair? ls)
                (cons-cell-count-cps (car ls)
                    (lambda (v1)
                        (match v1
                            [`,y #:when (number? y) (cons-cell-count-cps (cdr ls) 
                                (lambda (v2) 
                                    (match v2
                                        [`,y #:when (number? y) (k (add1 (+ v1 v2)))])))])))]
            [else (match (k 0)
                    [`,y #:when (number? y) y])])))


; (trace cons-cell-count-cps)


; ; test
; (cons-cell-count-cps '(1 2) (empty-k))

; 7 find-cps
(: find (-> Any (Listof (Pairof Any Any)) Any))
(define find 
    (lambda (u s)
        (let ((pr (assv u s)))
            (if pr (find (cdr pr) s) u))))

(: find-cps (-> Any (Listof (Pairof Any Any)) (-> Any Any) Any))
(define find-cps 
    (lambda (u s k)
        (let ((pr (assv u s)))
            (if pr 
                (find-cps (cdr pr) s (lambda (v) k v)) 
                (k u)))))

; test
; (find-cps 7 '((5 . a) (6 . 5) (7 . 6)) (empty-k))

; 8 ack-cps
;; ack: computes the Ackermann function
;; (http://en.wikipedia.org/wiki/Ackermann_function).  Warning: if you
;; run this program with m >= 4 and n >= 2, you'll be in for a long
;; wait.
(: ack (-> Number Number Number))
(define ack
    (lambda (m n)
        (cond
            [(zero? m) (add1 n)]
            [(zero? n) (ack (sub1 m) 1)]
            [else (ack (sub1 m)
                (ack m (sub1 n)))])))


(: ack-cps (-> Number Number Countinuation Number))
(define ack-cps
    (lambda (m n k)
        (cond
            [(zero? m) (k (add1 n))]
            [(zero? n) (ack-cps (sub1 m) 1 k)]
            [else (ack-cps m (sub1 n)
                (lambda (v) (ack-cps (sub1 m) v k)))])))

; test
; (ack-cps 2 4 (empty-k))

; 9 fib-cps

(define fib
    (lambda (n)
        ((lambda (fib)
            (fib fib n))
        (lambda (fib n)
            (cond
                [(zero? n) 0]
                [(zero? (sub1 n)) 1]
                [else (+ (fib fib (sub1 n)) (fib fib (sub1 (sub1 n))))])))))

(define fib-cps
    (lambda (n k)
        ((lambda (fib k)
            (fib fib n k))
        (lambda (fib n k)
            (cond
                [(zero? n) (k 0)]
                [(zero? (sub1 n)) (k 1)]
                [else (fib fib (sub1 n) 
                    (lambda (v1)
                        (fib fib (sub1 (sub1 n))
                            (lambda (v2)
                                (k (+ v1 v2))))))])) k)))

; test
; (fib-cps 6 (empty-k))

; 10 unfold-cps
(define unfold
  (lambda (p f g seed)
    ((lambda (h)
       ((h h) seed '()))
     (lambda (h)
       (lambda (seed ans)
	 (if (p seed)
	     ans
	     ((h h) (g seed) (cons (f seed) ans))))))))

; helpers
(define null?-cps
    (lambda (ls k)
      (k (null? ls))))

(define car-cps
    (lambda (pr k)
      (k (car pr))))

(define cdr-cps
    (lambda (pr k)
      (k (cdr pr))))


(define unfold-cps
  (lambda (p f g seed k)
    ((lambda (h k)
       (h h (lambda (v) (v seed '() k))))
     (lambda (h k)
       (k (lambda (seed ans k)
        (p seed 
            (lambda (v)
                (if v 
                    (k ans)
                    (h h
                        (lambda (hh)
                            (f seed 
                                (lambda (v1)
                                    (g seed (lambda (v2)
                                        (hh v2 (cons v1 ans) k)))))))))))))k)))

; 11 unify-cps

(define empty-s
  (lambda ()
    '()))
 
(define unify
  (lambda (u v s)
    (cond
      ((eqv? u v) s)
      ((number? u) (cons (cons u v) s))
      ((number? v) (unify v u s))
      ((pair? u)
       (if (pair? v)
	   (let ((s (unify (find (car u) s) (find (car v) s) s)))
             (if s (unify (find (cdr u) s) (find (cdr v) s) s) #f))
	   #f))
      (else #f))))

(define unify-cps
  (lambda (u v s k)
    (cond
      ((eqv? u v) (k s))
      ((number? u) (k (cons (cons u v) s)))
      ((number? v) (unify-cps v u s k))
      ((pair? u)
        (if (pair? v)
	    (find-cps (car u) s (lambda (v-fu) 
            (find-cps (car v) s (lambda (v-fv)
                (unify-cps v-fu v-fv s (lambda (s)
                    (if s
                        (find-cps (cdr u) s (lambda (v-fu^)
                            (find-cps (cdr v) s (lambda (v-fv^)
                                (unify-cps v-fu^ v-fv^ s k)))))
                        #f)))))))
    ;    #;
    ;    (let ((s (unify-cps (find (car u) s) (find (car v) s) s)))
    ;          (if s (unify-cps (find (cdr u) s) (find (cdr v) s) s) #f))
	   #f))
      (else #f))))

; 12 M-cps
(define M
    (lambda (f)
        (lambda (ls)
            (cond
                ((null? ls) '())
                (else (cons (f (car ls)) ((M f) (cdr ls))))))))

; test
; ((M null?) '(1 2 ()))

(define M-cps
    (lambda (f k1)
        (k1 (lambda (ls k2)
            (cond
                ((null? ls) (k2 '()))
                (else (M-cps f (lambda (v1)
                    (v1 (cdr ls) (lambda (v2) 
                        (f (car ls) (lambda (v3)
                            (k2 (cons v3 v2))))))))))))))


; test
; ((M-cps null?-cps (empty-k)) '(1 2 ()) (empty-k))

; 13 use-of-M-cps
(define use-of-M
  ((M (lambda (n) (add1 n))) '(1 2 3 4 5)))

(define use-of-M-cps
  ((M-cps (lambda (n k) (k (add1 n))) (empty-k)) '(1 2 3 4 5) (empty-k)))


; 14 strange-cps
(define strange
  (lambda (x)
    ((lambda (g) (lambda (x) (g g)))
     (lambda (g) (lambda (x) (g g))))))

(define strange-cps
  (lambda (x k)
    ((lambda (g k) (k (lambda (x k) (g g k))))
     (lambda (g k) (k (lambda (x k) (g g k))))
     (lambda (k) k))))


; 15 use-of-strange-cps
(define use-of-strange
  (let ([strange^ (((strange 5) 6) 7)])
    (((strange^ 8) 9) 10)))

(define use-of-strange-cps
  (let ([strange^ (((strange-cps 5 (empty-k)) 6 (empty-k)) 7 (empty-k))])
    (((strange^ 8 (empty-k)) 9 (empty-k)) 10 (empty-k))))

; 16 why-cps
(define why
  (lambda (f)
    ((lambda (g)
       (f (lambda (x) ((g g) x))))
     (lambda (g)
       (f (lambda (x) ((g g) x)))))))

(define almost-length
    (lambda (f)
      (lambda (ls)
        (if (null? ls)
            0
            (add1 (f (cdr ls)))))))

(define why-cps
  (lambda (f k)
    ((lambda (g k)
       (f (lambda (x k) (g g (lambda (v) (v x k)))) k))
     (lambda (g k)
       (f (lambda (x k) (g g (lambda (v) (v x k)))) k)) k)))


(define almost-length-cps
    (lambda (f k)
      (k (lambda (ls k)
        (if (null? ls)
            (k 0)
            (f (cdr ls) (lambda (v)
                (k (add1 v)))))))))

; 17 why-cps-cps
(define why-cps-cps
  (lambda (f k c)
    ((lambda (g k c)
       (f (lambda (x k c) (g g (lambda (v c) (v x k)))) k))
     (lambda (g k c)
       (f (lambda (x k c) (g g (lambda (v c) (v x k)))) k)) k)))

