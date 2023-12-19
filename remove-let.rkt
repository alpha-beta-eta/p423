#lang racket
(require "utils.rkt")
;; <program> ::= (letrec ((<label> (lambda (<uvar> <uvar>*) <body>))*) <body>)
;; <value> ::= <triv>
;;          |  (if <pred> <value> <value>)
;;          |  (begin <effect>* <value>)
;;          |  (let ((<uvar> <value>)*) <value>)
;;          |  (alloc <value>)
;;          |  (mref <value> <value>)
;;          |  (<binop> <value> <value>)
;;          |  (<value> <value> <value>*)
;; <pred> ::= (true)
;;         |  (false)
;;         |  (if <pred> <pred> <pred>)
;;         |  (begin <effect>* <pred>)
;;         |  (let ((<uvar> <value>)*) <pred>)
;;         |  (<relop> <value> <value>)
;; <effect> ::= (nop)
;;           |  (if <pred> <effect> <effect>)
;;           |  (begin <effect>* <effect>)
;;           |  (let ((<uvar> <value>)*) <effect>)
;;           |  (mset! <value> <value> <value>)
;;           |  (<value> <value> <value>*)
;; <body> ::= (locals (<uvar>*) <value>)
;; <triv> ::= <int> | <uvar> | <label>
;; <binop> ::= + | - | * | sra | logand
;; <relop> ::= = | < | > | <= | >=
(define (remove-let program)
  (define (Value value)
    (match value
      (,triv (guard (not (pair? triv))) triv)
      ((if ,pred ,v1 ,v2)
       `(if ,(Pred pred)
            ,(Value v1)
            ,(Value v2)))
      ((begin . ,exp+)
       (:begin exp+
               (lambda (exp* exp)
                 (let ((exp* (map Effect exp*))
                       (exp (Value exp)))
                   (make-begin exp* exp)))))
      ((let ,bds ,body)
       (: bds
          (lambda (x* e*)
            (let ((e* (map Value e*))
                  (body (Value body)))
              