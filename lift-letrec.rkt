#lang racket
(provide lift-letrec)
(require "utils.rkt")
;; <exp> ::= (quote <immediate>)
;;        |  <uvar>
;;        |  <label>
;;        |  (if <exp> <exp> <exp>)
;;        |  (begin <exp>+)
;;        |  (let ((<uvar> <exp>)*) <exp>)
;;        |  (letrec ((<label> (lambda (<uvar> <uvar>*) <exp>))*) <exp>)
;;        |  (<prim> <exp>*)
;;        |  (<exp> <exp> <exp>*)
;; The set of primitives has been changed.
(define (lift-letrec exp)
  (define (Lam lam k)
    (match lam
      ((lambda ,x* ,body)
       (lift body
             (lambda (body binding*)
               (k `(lambda ,x* ,body)
                  binding*))))))
  (define Lam* (make-proc* Lam append))
  (define (lift exp k)
    (match exp
      ((quote ,i) (k exp '()))
      ;x may be a uvar or a label
      (,x (guard (symbol? x)) (k x '()))
      ((if ,q ,a ,e)
       (lift* (cdr exp)
              (lambda (exp* binding*)
                (k (cons 'if exp*)
                   binding*))))
      ((begin . ,exp*)
       (lift* exp*
              (lambda (exp* binding*)
                (k (cons 'begin exp*)
                   binding*))))
      ((let ,bds ,body)
       (: bds
          (lambda (x* e*)
            (lift*
             e* (lambda (e* binding*0)
                  (lift body
                        (lambda (body binding*1)
                          (k (Let x* e* body)
                             (append binding*0
                                     binding*1)))))))))
      ((letrec ,bds ,body)
       (: bds
          (lambda (x* e*)
            (Lam*
             e* (lambda (e* binding*0)
                  (lift body
                        (lambda (body binding*1)
                          (k body
                             (append
                              (map list x* e*)
                              binding*0
                              binding*1)))))))))
      ((,prim . ,rands)
       (guard (prim+? prim))
       (lift* rands
              (lambda (rands binding*)
                (k (cons prim rands)
                   binding*))))
      ((,rator . ,rands)
       (lift* exp k))))
  (define lift* (make-proc* lift append))
  (lift exp
        (lambda (exp binding*)
          `(letrec ,binding* ,exp))))
;; <program> ::= (letrec ((<label> (lambda (<uvar> <uvar>*) <exp>))*) <exp>)
;; <exp> ::= (quote <immediate>)
;;        |  <uvar>
;;        |  <label>
;;        |  (if <exp> <exp> <exp>)
;;        |  (begin <exp>+)
;;        |  (let ((<uvar> <exp>)*) <exp>)
;;        |  (<prim> <exp>*)
;;        |  (<exp> <exp> <exp>*)