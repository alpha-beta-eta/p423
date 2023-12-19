#lang racket
(provide uncover-locals)
(require "utils.rkt")
;; <program> ::= (letrec ((<label> (lambda (<uvar> <uvar>*) <value>))*) <value>)
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
;; <triv> ::= <int> | <uvar> | <label>
;; <binop> ::= + | - | * | sra | logand
;; <relop> ::= = | < | > | <= | >=
(define (uncover-locals program)
  (define (make-begin exp* exp)
    `(begin ,@exp* ,exp))
  (define (If Context pred e1 e2 k)
    (Pred
     pred
     (lambda (pred a*)
       (Context
        e1
        (lambda (e1 b*)
          (Context
           e2
           (lambda (e2 c*)
             (k `(if ,pred ,e1 ,e2)
                (append a* b* c*)))))))))
  (define (Begin Context exp+ k)
    (:begin exp+
            (lambda (exp* exp)
              (Effect*
               exp*
               (lambda (exp* a*)
                 (Context
                  exp
                  (lambda (exp b*)
                    (k (make-begin exp* exp)
                       (append a* b*)))))))))
  (define (Leto Context bds body k)
    (: bds
       (lambda (x* e*)
         (Value*
          e*
          (lambda (e* a*)
            (Context
             body
             (lambda (body b*)
               (k (Let x* e* body)
                  (append x* a* b*)))))))))
  (define (Op k op . v*)
    (Value* v* (lambda (v* a*)
                 (k (cons op v*) a*))))
  (define (Value value k)
    (match value
      (,triv
       (guard (not (pair? triv)))
       (k triv '()))
      ((if ,pred ,v1 ,v2) (If Value pred v1 v2 k))
      ((begin . ,exp+) (Begin Value exp+ k))
      ((let ,bds ,body) (Leto Value bds body k))
      ((alloc ,v) (Op k 'alloc v))
      ((mref ,v1 ,v2) (Op k 'mref v1 v2))
      ((,binop ,v1 ,v2)
       (guard (binop? binop))
       (Op k binop v1 v2))
      ((,rator . ,rands) (Value* value k))))
  (define (Pred pred k)
    (match pred
      ((true) (k '(true) '()))
      ((false) (k '(false) '()))
      ((if ,p1 ,p2 ,p3) (If Pred p1 p2 p3 k))
      ((begin . ,exp+) (Begin Pred exp+ k))
      ((let ,bds ,body) (Leto Pred bds body k))
      ((,relop ,v1 ,v2)
       (guard (relop? relop))
       (Op k relop v1 v2))))
  (define (Effect effect k)
    (match effect
      ((nop) (k '(nop) '()))
      ((if ,pred ,e1 ,e2) (If Effect pred e1 e2 k))
      ((begin . ,exp+) (Begin Effect exp+ k))
      ((let ,bds ,body) (Leto Effect bds body k))
      ((mset! ,v1 ,v2 ,v3) (Op k 'mset! v1 v2 v3))
      ((,rator . ,rands) (Value* effect k))))
  (define Value* (make-proc* Value append))
  (define Effect* (make-proc* Effect append))
  (define (make-body value)
    (Value value
           (lambda (value a*)
             `(locals ,a* ,value))))
  (match program
    ((letrec ,bds ,body)
     (: bds
        (lambda (x* e*)
          (let ((e* (map (lambda (e)
                           (match e
                             ((lambda ,x* ,body)
                              `(lambda ,x* ,(make-body body)))))
                         e*))
                (body (make-body body)))
            (Letrec x* e* body)))))))
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