#lang racket
(provide introduce-procedure-primitives)
(require "utils.rkt")
;; <exp> ::= (quote <immediate>)
;;        |  <uvar>
;;        |  (if <exp> <exp> <exp>)
;;        |  (begin <exp>+)
;;        |  (let ((<uvar> <exp>)*) <exp>)
;;        |  (letrec ((<label> (lambda (<uvar> <uvar>*)
;;                               (bind-free (<uvar> <uvar>*) <exp>)))*)
;;             (closures ((<uvar> <label> <uvar>*)*) <exp>))
;;        |  (<prim> <exp>*)
;;        |  (<label> <exp> <exp>*)
;;        |  (<exp> <exp> <exp>*)
(define (introduce-procedure-primitives exp)
  (define (intro exp)
    (match exp
      ((quote ,i) exp)
      (,x (guard (symbol? x)) x)
      ((if ,q ,a ,e)
       (cons 'if (map intro (cdr exp))))
      ((begin . ,exp*)
       (cons 'begin (map intro exp*)))
      ((let ,bds ,body)
       (: bds
          (lambda (x* e*)
            (let ((e* (map intro e*))
                  (body (intro body)))
              (Let x* e* body)))))
      ((letrec ,bds (closures ,closures ,body))
       (define make-procedure*
         (map
          (lambda (c)
            (match c
              ((,name ,label . ,free*)
               `(,name (make-procedure ,label (quote ,(length free*)))))))
          closures))
       (define procedure-set!*
         (append-map
          (lambda (c)
            (match c
              ((,name ,label . ,free*)
               (mapi (lambda (free index)
                       `(procedure-set! ,name (quote ,index) ,free))
                     free*))))
          closures))
       (: bds
          (lambda (x* e*)
            (let ((e* (map (lambda (e)
                             (match e
                               ((lambda ,x*
                                  (bind-free
                                   (,cp . ,free*)
                                   ,body))
                                `(lambda ,x* ,((Intro cp free*) body)))))
                           e*))
                  (body (intro body)))
              (Letrec
               x* e*
               `(let ,make-procedure*
                  ,(if (null? procedure-set!*)
                       body
                       `(begin ,@procedure-set!* ,body))))))))
      ((,prim . ,rands)
       (guard (prim? prim))
       (cons prim (map intro rands)))
      ((,label . ,rands)
       (guard (label? label))
       (cons label (map intro rands)))
      ((,rator . ,rands)
       (let ((rator (intro rator))
             (rands (map intro rands)))
         `((procedure-code ,rator) . ,rands)))))
  (define (lookup x cp free*)
    (let walk ((i 0) (free* free*))
      (cond ((null? free*) x)
            ((eq? x (car free*))
             `(procedure-ref ,cp (quote ,i)))
            (else
             (walk (+ i 1) (cdr free*))))))
  (define (Intro cp free*)
    (lambda (exp)
      (match exp
        ((quote ,i) exp)
        (,x (guard (symbol? x)) (lookup x cp free*))
        ((if ,q ,a ,e)
         (cons 'if (map (Intro cp free*) (cdr exp))))
        ((begin . ,exp*)
         (cons 'begin (map (Intro cp free*) exp*)))
        ((let ,bds ,body)
         (: bds
            (lambda (x* e*)
              (let ((e* (map (Intro cp free*) e*))
                    (body ((Intro cp free*) body)))
                (Let x* e* body)))))
        ((letrec ,bds (closures ,closures ,body))
         (define make-procedure*
           (map
            (lambda (c)
              (match c
                ((,name ,label . ,free*)
                 `(,name (make-procedure ,label (quote ,(length free*)))))))
            closures))
         (define procedure-set!*
           (append-map
            (lambda (c)
              (match c
                ((,name ,label . ,free*)
                 (mapi (lambda (free index)
                         `(procedure-set! ,name (quote ,index) ,free))
                       free*))))
            closures))
         (: bds
            (lambda (x* e*)
              (let ((e* (map (lambda (e)
                               (match e
                                 ((lambda ,x*
                                    (bind-free
                                     (,cp . ,free*)
                                     ,body))
                                  `(lambda ,x* ,((Intro cp free*) body)))))
                             e*))
                    (body ((Intro cp free*) body)))
                (Letrec
                 x* e*
                 `(let ,make-procedure*
                    ,(if (null? procedure-set!*)
                         body
                         `(begin ,@procedure-set!* ,body))))))))
        ((,prim . ,rands)
         (guard (prim? prim))
         (cons prim (map (Intro cp free*) rands)))
        ((,label . ,rands)
         (guard (label? label))
         (cons label (map (Intro cp free*) rands)))
        ((,rator . ,rands)
         (let ((rator ((Intro cp free*) rator))
               (rands (map (Intro cp free*) rands)))
           `((procedure-code ,rator) . ,rands))))))
  (intro exp))
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