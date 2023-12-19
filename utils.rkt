#lang racket
(provide (except-out (all-defined-out) ppat))
(define-syntax match
  (syntax-rules (guard)
    ((_ v) (error 'match "~s" v))
    ((_ v (pat (guard g ...) e ...) cs ...)
     (let ((fk (lambda () (match v cs ...))))
       (ppat v pat (if (and g ...) (let () e ...) (fk)) (fk))))
    ((_ v (pat e ...) cs ...)
     (let ((fk (lambda () (match v cs ...))))
       (ppat v pat (let () e ...) (fk))))))
(define-syntax ppat
  (syntax-rules (unquote)
    ((_ v () kt kf) (if (null? v) kt kf))
    ((_ v (unquote var) kt kf) (let ((var v)) kt))
    ((_ v (x . y) kt kf)
     (if (pair? v)
         (let ((vx (car v)) (vy (cdr v)))
           (ppat vx x (ppat vy y kt kf) kf))
         kf))
    ((_ v lit kt kf) (if (eqv? v (quote lit)) kt kf))))
(define (make-counter x)
  (lambda ()
    (set! x (+ x 1))
    x))
(define unique-symbol
  (let ((counter (make-counter -1)))
    (lambda (x)
      (string->symbol
       (format "~s.~s" x (counter))))))
(define (set? x)
  (cond ((null? x) #t)
        ((memq (car x) (cdr x)) #f)
        (else (set? (cdr x)))))
(define (set-cons x s)
  (if (memq x s)
      s
      (cons x s)))
(define (U s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (U (cdr s1) s2))
        (else (cons (car s1) (U (cdr s1) s2)))))
(define (I s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (cons (car s1) (I (cdr s1) s2)))
        (else (I (cdr s1) s2))))
(define (D s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (D (cdr s1) s2))
        (else (cons (car s1) (D (cdr s1) s2)))))
(define (target-fixnum? x)
  (and (integer? x)
       (exact? x)
       (<= (- (expt 2 60)) x (- (expt 2 60) 1))))
(define (: bds k)
  (k (map car bds) (map cadr bds)))
(define (Let x* e* body)
  (list 'let (map list x* e*) body))
(define (Letrec x* e* body)
  (list 'letrec (map list x* e*) body))
(define (datum? x)
  (or (null? x) (boolean? x) (target-fixnum? x)
      (and (pair? x) (datum? (car x)) (datum? (cdr x)))
      (and (vector? x)
           (let ((len (vector-length x)))
             (let loop ((i 0))
               (cond ((= len i) #t)
                     ((datum? (vector-ref x i)) (loop (+ i 1)))
                     (else #f)))))))
(define-syntax push!
  (syntax-rules ()
    ((_ x l) (set! l (cons x l)))))
(define prims
  '(+
    -
    *
    =
    <
    >
    <=
    >=
    null?
    boolean?
    fixnum?
    pair?
    vector?
    box?
    procedure?
    eq?
    not
    cons
    car
    cdr
    set-car!
    set-cdr!
    make-vector
    vector-length
    vector-ref
    vector-set!
    box
    unbox
    set-box!
    void))
(define (prim? x)
  (if (memq x prims) #t #f))