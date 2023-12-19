#lang racket
(provide convert-closures)
(require "utils.rkt")
;; <exp> ::= (quote <immediate>)
;;        |  <uvar>
;;        |  (if <exp> <exp> <exp>)
;;        |  (begin <exp>+)
;;        |  (let ((<uvar> <exp>)*) <exp>)
;;        |  (letrec ((<uvar> (lambda (<uvar>*) (free (<uvar>*) <exp>)))*) <exp>)
;;        |  (<prim> <exp>*)
;;        |  (<exp> <exp>*)
(define (convert-closures exp)
  (define (convert exp)
    (match exp
      ((quote ,i) exp)
      (,x (guard (symbol? x)) x)
      ((if ,q ,a ,e)
       `(if ,(convert q)
            ,(convert a)
            ,(convert e)))
      ((begin . ,exp*)
       (cons 'begin (map convert exp*)))
      ((let ,bds ,body)
       (: bds
          (lambda (x* e*)
            (let ((e* (map convert e*))
                  (body (convert body)))
              (Let x* e* body)))))
      ((letrec ,bds ,body)
       (: bds
          (lambda (x* e*)
            (define label*
              (map unique-label x*))
            (define lam*
              (map (lambda (e)
                     (match e
                       ((lambda ,x* (free ,f* ,body))
                        (define cp
                          (unique-symbol 'cp))
                        `(lambda ,(cons cp x*)
                           (bind-free
                            ,(cons cp f*)
                            ,(convert body))))))
                   e*))
            (define free**
              (map (lambda (e)
                     (match e
                       ((lambda ,x* (free ,f* ,body))
                        f*)))
                   e*))
            (define closure*
              (map (lambda (x label free*)
                     `(,x ,label . ,free*))
                   x* label* free**))
            (Letrec
             label* lam*
             `(closures ,closure* ,(convert body))))))
      ((,prim . ,exp*)
       (guard (prim? prim))
       (cons prim (map convert exp*)))
      ((,rator . ,rand*)
       (let ((rator (convert rator))
             (rand* (map convert rand*)))
         (if (symbol? rator)
             `(,rator ,rator . ,rand*)
             (let ((t (unique-symbol 't)))
               `(let ((,t ,rator))
                  (,t ,t . ,rand*))))))))
  (convert exp))
;; <exp> ::= (quote <immediate>)
;;        |  <uvar>
;;        |  (if <exp> <exp> <exp>)
;;        |  (begin <exp>+)
;;        |  (let ((<uvar> <exp>)*) <exp>)
;;        |  (letrec ((<label> (lambda (<uvar> <uvar>*)
;;                               (bind-free (<uvar> <uvar>*) <exp>)))*)
;;             (closures ((<uvar> <label> <uvar>*)*) <exp>))
;;        |  (<prim> <exp>*)
;;        |  (<exp> <exp> <exp>*)