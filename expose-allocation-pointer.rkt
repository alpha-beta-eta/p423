#lang racket
(provide expose-allocation-pointer)
(require "utils.rkt")
;; <program> ::= (letrec ((<label> (lambda () <body>))*) <body>)
;; <tail> ::= (if <pred> <tail> <tail>)
;;         |  (begin <effect>* <tail>)
;;         |  (<triv> <var>*)
;; <pred> ::= (true)
;;         |  (false)
;;         |  (if <pred> <pred> <pred>)
;;         |  (begin <effect>* <pred>)
;;         |  (<relop> <triv> <triv>)
;; <effect> ::= (nop)
;;           |  (if <pred> <effect> <effect>)
;;           |  (begin <effect>* <effect>)
;;           |  (set! <var> <simple>)
;;           |  (return-point <label>
;;                (begin (set! <var> <triv>)* (<triv> <var>*)))
;;           |  (mset! <triv> <triv> <triv>)
;; <simple> ::= <triv>
;;           |  (alloc <triv>)
;;           |  (mref <triv> <triv>)
;;           |  (<binop> <triv> <triv>)
;; <body> ::= (locals (<uvar>*)
;;              (new-frames (<frame>*) <tail>))
;; <frame> ::= (<uvar>*)
;; <triv> ::= <int> | <var> | <label>
;; <var> ::= <uvar> | <loc>
;; <loc> ::= <reg> | <fvar>
;; <binop> ::= + | - | * | sra | logand
;; <relop> ::= = | < | > | <= | >=
(define (expose-allocation-pointer program)
  (define (If Context pred e1 e2)
    `(if ,(Pred pred) ,(Context e1) ,(Context e2)))
  (define (Begin Context exp+)
    (:begin exp+
            (lambda (exp* exp)
              (let ((exp* (map Effect exp*))
                    (exp (Context exp)))
                (make-begin exp* exp)))))
  (define (Tail tail)
    (match tail
      ((if ,pred ,t1 ,t2) (If Tail pred t1 t2))
      ((begin . ,exp+) (Begin Tail exp+))
      ((,rator . ,rands) tail)))
  (define (Pred pred)
    (match pred
      ((true) '(true))
      ((false) '(false))
      ((if ,p1 ,p2 ,p3) (If Pred p1 p2 p3))
      ((begin . ,exp+) (Begin Pred exp+))
      ((,relop ,x ,y) (guard (relop? relop)) pred)))
  (define (Effect effect)
    (match effect
      ((nop) '(nop))
      ((if ,pred ,e1 ,e2) (If Effect pred e1 e2))
      ((begin . ,exp+) (Begin Effect exp+))
      ((set! ,x ,simple)
       (match simple
         ((alloc ,size)
          `(begin (set! ,x ,allocation-pointer-register)
                  (set! ,allocation-pointer-register
                        (+ ,allocation-pointer-register ,size))))
         (,else effect)))
      ((return-point . ,rest) effect)
      ((mset! ,x ,y ,z) effect)))
  (define (Body body)
    (match body
      ((locals ,x* (new-frames ,frame* ,tail))
       `(locals ,x* (new-frames ,frame* ,(Tail tail))))))
  (match program
    ((letrec ,bds ,body)
     (: bds
        (lambda (x* e*)
          (let ((e* (map (lambda (e)
                           (match e
                             ((lambda () ,body)
                              `(lambda () ,(Body body)))))
                         e*))
                (body (Body body)))
            (Letrec x* e* body)))))))
;; <program> ::= (letrec ((<label> (lambda () <body>))*) <body>)
;; <tail> ::= (if <pred> <tail> <tail>)
;;         |  (begin <effect>* <tail>)
;;         |  (<triv> <var>*)
;; <pred> ::= (true)
;;         |  (false)
;;         |  (if <pred> <pred> <pred>)
;;         |  (begin <effect>* <pred>)
;;         |  (<relop> <triv> <triv>)
;; <effect> ::= (nop)
;;           |  (if <pred> <effect> <effect>)
;;           |  (begin <effect>* <effect>)
;;           |  (set! <var> <simple>)
;;           |  (return-point <label>
;;                (begin (set! <var> <triv>)* (<triv> <var>*)))
;;           |  (mset! <triv> <triv> <triv>)
;; <simple> ::= <triv>
;;           |  (mref <triv> <triv>)
;;           |  (<binop> <triv> <triv>)
;; <body> ::= (locals (<uvar>*)
;;              (new-frames (<frame>*) <tail>))
;; <frame> ::= (<uvar>*)
;; <triv> ::= <int> | <var> | <label>
;; <var> ::= <uvar> | <loc>
;; <loc> ::= <reg> | <fvar>
;; <binop> ::= + | - | * | sra | logand
;; <relop> ::= = | < | > | <= | >=