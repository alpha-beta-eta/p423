#lang racket
(provide uncover-free)
(require "utils.rkt")
;; <exp> ::= (quote <immediate>)
;;        |  <uvar>
;;        |  (if <exp> <exp> <exp>)
;;        |  (begin <exp>+)
;;        |  (let ((<uvar> <exp>)*) <exp>)
;;        |  (letrec ((<uvar> (lambda (<uvar>*) <exp>))*) <exp>)
;;        |  (<prim> <exp>*)
;;        |  (<exp> <exp>*)
(define (uncover-free exp)
  (define (Lam lam k)
    (match lam
      ((lambda ,x* ,body)
       (uncover
        body
        (lambda (body u*)
          (define v* (D u* x*))
          (k `(lambda ,x*
                (free ,v* ,body))
             v*))))))
  (define Lam* (make-proc* Lam))
  (define (uncover exp k)
    (match exp
      ((quote ,i) (k exp '()))
      (,x (guard (symbol? x)) (k x (list x)))
      ((if ,q ,a ,e)
       (uncover*
        (cdr exp)
        (lambda (qae u*)
          (k (cons 'if qae) u*))))
      ((begin . ,exp*)
       (uncover*
        exp*
        (lambda (exp* u*)
          (k (cons 'begin exp*) u*))))
      ((let ,bds ,body)
       (: bds
          (lambda (x* e*)
            (uncover*
             e* (lambda (e* u*)
                  (uncover
                   body
                   (lambda (body v*)
                     (k (Let x* e* body)
                        (U u* (D v* x*))))))))))
      ((letrec ,bds ,body)
       (: bds
          (lambda (x* e*)
            (Lam*
             e* (lambda (e* u*)
                  (uncover
                   body
                   (lambda (body v*)
                     (k (Letrec x* e* body)
                        (D (U u* v*) x*)))))))))
      ((,prim . ,rands)
       (guard (prim? prim))
       (uncover* rands
                 (lambda (rands u*)
                   (k (cons prim rands) u*))))
      ((,rator . ,rands) (uncover* exp k))))
  (define uncover* (make-proc* uncover))
  (uncover
   exp (lambda (exp u*)
         (unless (null? u*)
           (error 'uncover-free
                  "unbound variables ~s" u*))
         exp)))
;; <exp> ::= (quote <immediate>)
;;        |  <uvar>
;;        |  (if <exp> <exp> <exp>)
;;        |  (begin <exp>+)
;;        |  (let ((<uvar> <exp>)*) <exp>)
;;        |  (letrec ((<uvar> (lambda (<uvar>*) (free (<uvar>*) <exp>)))*) <exp>)
;;        |  (<prim> <exp>*)
;;        |  (<exp> <exp>*)