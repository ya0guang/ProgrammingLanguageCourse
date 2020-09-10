#lang racket

#|
TAKE-AWAYS
1, higher-order function: you can pass one function as the input of another function,
as well as return a function as the result of another function
2, the Ackermann function (G): the first discovered non-primitive recursion that is guaranteed to
terminate.
|#

#;
(define +
  (λ (x y)
    (cond
      [(zero? y) x]
      [else (add1 (+ x (sub1 y)))])))

#;
(define *
  (λ (x y)
    (cond
      [(zero? y) 0]
      [else (+ x (* x (sub1 y)))])))

#;
(define ↑
  (λ (x y)
    (cond
      [(zero? y) 1]
      [else (* x (↑ x (sub1 y)))])))

#;
(define ⇑
  (λ (x y)
    (cond
      [(zero? y) 1]
      [else (↑ x (⇑ x (sub1 y)))])))


(define G
  (λ (i)
    (λ (x y)
      (cond
        #|this line gotta be +|#
        [(zero? i)
         (cond
           [(zero? y) x]
           [else (add1 ((G i) x (sub1 y)))])]
        #|this line gotta be *|#
        [(eqv? i 1)
         (cond
           [(zero? y) 0]
           [else ((G (sub1 i)) x ((G i) x (sub1 y)))])]
        #|↑ and the above|#
        [else
         (cond
           [(zero? y) 1]
           [else ((G (sub1 i)) x ((G i) x (sub1 y)))])]))))

(define times5
  (λ (n)
    ((G 1) 5 n)))

(define map
  (λ (l f)
    (cond
      [(null? l) '()]
      [else (cons (f (car l)) (map (cdr l) f))])))
