#lang racket
(provide remove-complex-opera*)
(require "utils.rkt")
;; <program> ::= (letrec ((<label> (lambda (<uvar>*) <body>))*) <body>)
;; <value> ::= <triv>
;;          |  (if <pred> <value> <value>)
;;          |  (begin <effect>* <value>)
;;          |  (alloc <value>)
;;          |  (mref <value> <value>)
;;          |  (<binop> <value> <value>)
;;          |  (<value> <value>*)
;; <pred> ::= (true)
;;         |  (false)
;;         |  (if <pred> <pred> <pred>)
;;         |  (begin <effect>* <pred>)
;;         |  (<relop> <value> <value>)
;; <effect> ::= (nop)
;;           |  (if <pred> <effect> <effect>)
;;           |  (begin <effect>* <effect>)
;;           |  (set! <uvar> <value>)
;;           |  (mset! <value> <value> <value>)
;;           |  (<value> <value>*)
;; <tail> ::= <triv>
;;         |  (if <pred> <tail> <tail>)
;;         |  (begin <effect>* <tail>)
;;         |  (alloc <value>)
;;         |  (mref <value> <value>)
;;         |  (<binop> <value> <value>)
;;         |  (<value> <value>*)
;; <body> ::= (locals (<uvar>*) <tail>)
;; <triv> ::= <int> | <uvar> | <label>
;; <binop> ::= + | - | * | sra | logand
;; <relop> ::= = | < | > | <= | >=
(define (remove-complex-opera* program)
  (define (trivialize value k)
    (match value
      (,triv
       (guard (not (pair? triv)))
       (k triv '() '()))
      ((if ,pred ,v1 ,v2)
       (If Value pred v1 v2
           (lambda (i a*)
             (define tmp (unique-symbol 'tmp))
             (k tmp (cons tmp a*)
                (list `(set! ,tmp ,i))))))
      ((begin . ,exp+)
       (Begin Value exp+
              (lambda (b a*)
                (define tmp (unique-symbol 'tmp))
                (k tmp (cons tmp a*)
                   (list `(set! ,tmp ,b))))))
      ((alloc ,v) (trivialize-op k 'alloc v))
      ((mref ,v1 ,v2) (trivialize-op k 'mref v1 v2))
      ((,binop ,v1 ,v2)
       (guard (binop? binop))
       (trivialize-op k binop v1 v2))
      ((,rator . ,rands)
       (trivialize*
        value
        (lambda (triv* a* binding*)
          (define tmp (unique-symbol 'tmp))
          (k tmp (cons tmp a*)
             `(,@binding*
               (set! ,tmp ,triv*))))))))
  (define (trivialize* value* k)
    (if (null? value*)
        (k '() '() '())
        (trivialize
         (car value*)
         (lambda (triv a* binding*0)
           (trivialize*
            (cdr value*)
            (lambda (triv* b* binding*1)
              (k (cons triv triv*)
                 (append a* b*)
                 (append binding*0 binding*1))))))))
  (define (trivialize-op k op . v*)
    (trivialize*
     v* (lambda (triv* a* binding*)
          (define tmp (unique-symbol 'tmp))
          (k tmp (cons tmp a*)
             `(,@binding*
               (set! ,tmp ,(cons op triv*)))))))
  (define (Op k op . v*)
    (trivialize*
     v* (lambda (triv* a* binding*)
          (k (make-begin binding* (cons op triv*)) a*))))
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
  (define (Value value k)
    (match value
      (,triv
       (guard (not (pair? triv)))
       (k triv '()))
      ((if ,pred ,v1 ,v2) (If Value pred v1 v2 k))
      ((begin . ,exp+) (Begin Value exp+ k))
      ((alloc ,v) (Op k 'alloc v))
      ((mref ,v1 ,v2) (Op k 'mref v1 v2))
      ((,binop ,v1 ,v2)
       (guard (binop? binop))
       (Op k binop v1 v2))
      ((,rator . ,rands)
       (trivialize*
        value
        (lambda (triv* a* binding*)
          (k (make-begin binding* triv*) a*))))))
  (define (Pred pred k)
    (match pred
      ((true) (k '(true) '()))
      ((false) (k '(false) '()))
      ((if ,p1 ,p2 ,p3) (If Pred p1 p2 p3 k))
      ((begin . ,exp+) (Begin Pred exp+ k))
      ((,relop ,v1 ,v2)
       (guard (relop? relop))
       (Op k relop v1 v2))))
  (define (Effect effect k)
    (match effect
      ((nop) (k '(nop) '()))
      ((if ,pred ,e1 ,e2) (If Effect pred e1 e2 k))
      ((begin . ,exp+) (Begin Effect exp+ k))
      ((set! ,x ,v)
       (Value v (lambda (v a*)
                  (k `(set! ,x ,v) a*))))
      ((mset! ,v1 ,v2 ,v3) (Op k 'mset! v1 v2 v3))
      ((,rator . ,rands)
       (trivialize*
        effect
        (lambda (triv* a* binding*)
          (k (make-begin binding* triv*) a*))))))
  (define (Tail tail k)
    (match tail
      (,triv
       (guard (not (pair? triv)))
       (k triv '()))
      ((if ,pred ,t1 ,t2) (If Tail pred t1 t2 k))
      ((begin . ,exp+) (Begin Tail exp+ k))
      ((alloc ,v) (Op k 'alloc v))
      ((mref ,v1 ,v2) (Op k 'mref v1 v2))
      ((,binop ,v1 ,v2)
       (guard (binop? binop))
       (Op k binop v1 v2))
      ((,rator . ,rands)
       (trivialize*
        tail
        (lambda (triv* a* binding*)
          (k (make-begin binding* triv*) a*))))))
  (define Effect* (make-proc* Effect append))
  (define (Body body)
    (match body
      ((locals ,x* ,tail)
       (Tail tail
             (lambda (tail a*)
               `(locals ,(append x* a*) ,tail))))))
  (match program
    ((letrec ,bds ,body)
     (: bds
        (lambda (x* e*)
          (let ((e* (map (lambda (e)
                           (match e
                             ((lambda ,x* ,body)
                              `(lambda ,x* ,(Body body)))))
                         e*))
                (body (Body body)))
            (Letrec x* e* body)))))))
;; <program> ::= (letrec ((<label> (lambda (<uvar>*) <body>))*) <body>)
;; <value> ::= <triv>
;;          |  (if <pred> <value> <value>)
;;          |  (begin <effect>* <value>)
;;          |  (alloc <triv>)
;;          |  (mref <triv> <triv>)
;;          |  (<binop> <triv> <triv>)
;;          |  (<triv> <triv>*)
;; <pred> ::= (true)
;;         |  (false)
;;         |  (if <pred> <pred> <pred>)
;;         |  (begin <effect>* <pred>)
;;         |  (<relop> <triv> <triv>)
;; <effect> ::= (nop)
;;           |  (if <pred> <effect> <effect>)
;;           |  (begin <effect>* <effect>)
;;           |  (set! <uvar> <value>)
;;           |  (mset! <triv> <triv> <triv>)
;;           |  (<triv> <triv>*)
;; <tail> ::= <triv>
;;         |  (if <pred> <tail> <tail>)
;;         |  (begin <effect>* <tail>)
;;         |  (alloc <triv>)
;;         |  (mref <triv> <triv>)
;;         |  (<binop> <triv> <triv>)
;;         |  (<triv> <triv>*)
;; <body> ::= (locals (<uvar>*) <tail>)
;; <triv> ::= <int> | <uvar> | <label>
;; <binop> ::= + | - | * | sra | logand
;; <relop> ::= = | < | > | <= | >=