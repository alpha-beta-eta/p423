#lang racket
(provide impose-calling-conventions)
(require "utils.rkt")
;; <program> ::= (letrec ((<label> (lambda (<uvar>*) <body>))*) <body>)
;; <tail> ::= <triv>
;;         |  (if <pred> <tail> <tail>)
;;         |  (begin <effect>* <tail>)
;;         |  (alloc <triv>)
;;         |  (mref <triv> <triv>)
;;         |  (<binop> <triv> <triv>)
;;         |  (<triv> <triv>*)
;; <pred> ::= (true)
;;         |  (false)
;;         |  (if <pred> <pred> <pred>)
;;         |  (begin <effect>* <pred>)
;;         |  (<relop> <triv> <triv>)
;; <effect> ::= (nop)
;;           |  (if <pred> <effect> <effect>)
;;           |  (begin <effect>* <effect>)
;;           |  (set! <uvar> <simple>)
;;           |  (mset! <triv> <triv> <triv>)
;;           |  (<triv> <triv>*)
;; <simple> ::= <triv>
;;           |  (alloc <triv>)
;;           |  (mref <triv> <triv>)
;;           |  (<binop> <triv> <triv>)
;;           |  (<triv> <triv>*)
;; <body> ::= (locals (<uvar>*) <tail>)
;; <triv> ::= <int> | <uvar> | <label>
;; <binop> ::= + | - | * | sra | logand
;; <relop> ::= = | < | > | <= | >=
(define (impose-calling-conventions program)
  (define (triv? x)
    (not (pair? x)))
  (define prim?
    (make-predicate '(alloc mref + - * sra logand)))
  (define (make-nfv index)
    (unique-symbol 'nfv))
  (define (Body parameters body)
    (define rp (unique-symbol 'rp))
    (define (make-init)
      (let iter ((rest parameters)
                 (registers parameter-registers)
                 (index 0)
                 (result (list `(set! ,rp ,return-address-register))))
        (cond ((null? rest) (reverse result))
              ((null? registers)
               (iter (cdr rest) '() (add1 index)
                     (cons `(set! ,(car rest) ,(make-fv index))
                           result)))
              (else
               (iter (cdr rest) (cdr registers) index
                     (cons `(set! ,(car rest) ,(car registers))
                           result))))))
    (define new-frame-var** '())
    (define (extend! new-frame-var*)
      (set! new-frame-var**
            (cons new-frame-var* new-frame-var**)))
    (define (make-prep rator rands ret make)
      (let iter ((rest rands)
                 (registers parameter-registers)
                 (index 0)
                 (result1 '())
                 (result2 '())
                 (used* '())
                 (fv* '()))
        (cond ((null? rest)
               (values
                (make-begin
                 (append
                  (reverse result2)
                  (reverse result1)
                  (list `(set! ,return-address-register ,ret)))
                 `(,rator ,frame-pointer-register
                          ,return-address-register
                          ,@(reverse used*)
                          ,allocation-pointer-register))
                fv*))
              ((null? registers)
               (define fv (make index))
               (iter (cdr rest) '() (add1 index)
                     result1
                     (cons `(set! ,fv ,(car rest))
                           result2)
                     (cons fv used*)
                     (cons fv fv*)))
              (else
               (iter (cdr rest) (cdr registers) index
                     (cons `(set! ,(car registers) ,(car rest))
                           result1)
                     result2
                     (cons (car registers) used*)
                     fv*)))))
    (define (NonTailCall rator rands)
      (define ret-label (unique-label 'ret))
      (let-values (((call nfv*)
                    (make-prep rator rands ret-label make-nfv)))
        (extend! (reverse nfv*))
        `(return-point
          ,ret-label ,call)))
    (define (TailCall rator rands)
      (let-values (((call fv*)
                    (make-prep rator rands rp make-fv)))
        call))
    (define (If Context pred e1 e2)
      `(if ,(Pred pred) ,(Context e1) ,(Context e2)))
    (define (Begin Context exp+)
      (:begin exp+
              (lambda (exp* exp)
                (let ((exp* (map Effect exp*))
                      (exp (Context exp)))
                  (make-begin exp* exp)))))
    (define (TailNonCall tailNonCall)
      `(begin
         (set! ,return-value-register ,tailNonCall)
         (,rp ,frame-pointer-register
              ,return-value-register
              ,allocation-pointer-register)))
    (define (Tail tail)
      (match tail
        (,triv (guard (triv? triv)) (TailNonCall triv))
        ((if ,pred ,t1 ,t2) (If Tail pred t1 t2))
        ((begin . ,exp+) (Begin Tail exp+))
        ((,prim . ,x*) (guard (prim? prim)) (TailNonCall tail))
        ((,rator . ,rands) (TailCall rator rands))))
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
           (,triv (guard (triv? triv)) effect)
           ((,prim . ,x*) (guard (prim? prim)) effect)
           ((,rator . ,rands)
            `(begin ,(NonTailCall rator rands)
                    (set! ,x ,return-value-register)))))
        ((mset! ,x ,y ,z) effect)
        ((,rator . ,rands) (NonTailCall rator rands))))
    (match body
      ((locals ,x* ,tail)
       (define tail^ (Tail tail))
       `(locals
         ,(append x* (list rp) parameters
                  (apply append new-frame-var**))
         (new-frames
          ,new-frame-var**
          ,(make-begin (make-init) tail^))))))
  (match program
    ((letrec ,bds ,body)
     (: bds
        (lambda (x* e*)
          (let ((e* (map (lambda (e)
                           (match e
                             ((lambda ,x* ,body)
                              `(lambda ()
                                 ,(Body x* body)))))
                         e*))
                (body (Body '() body)))
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