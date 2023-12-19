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
(define counter0
  (make-counter -1))
(define (unique-symbol x)
  (string->symbol
   (format "~s.~s" x (counter0))))
(define (unique-label x)
  (string->symbol
   (format "~s$~s" x (counter0))))
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
(define (int64? x)
  (and (integer? x)
       (exact? x)
       (<= (- (expt 2 63)) x (- (expt 2 63) 1))))
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
(define (convert-datum x)
  (cond ((pair? x)
         (list 'cons (convert-datum (car x)) (convert-datum (cdr x))))
        ((vector? x)
         (let ((l (vector-length x)))
           (if (= l 0)
               '(make-vector '0)
               (let ((v (unique-symbol 'vec)))
                 `(let ((,v (make-vector (quote ,l))))
                    ,(let loop ((i l) (e* (cons v '())))
                       (if (= i 0)
                           (cons 'begin e*)
                           (let ((i (- i 1)))
                             (loop i
                                   (cons
                                    `(vector-set!
                                      ,v (quote ,i)
                                      ,(convert-datum (vector-ref x i)))
                                    e*))))))))))
        (else `(quote ,x))))
(define-syntax push!
  (syntax-rules ()
    ((_ x l) (set! l (cons x l)))))
(define (make-predicate x*)
  (lambda (x)
    (if (memq x x*) #t #f)))
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
    ;not
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
(define prim?
  (make-predicate prims))
(define (mapi f l)
  (let m ((i 0) (l l))
    (if (null? l)
        '()
        (cons (f (car l) i)
              (m (+ i 1) (cdr l))))))
(define uvar?
  (lambda (x)
    (and (symbol? x)
         (let* ([s (symbol->string x)] [n (string-length s)])
           (define (s0 i)
             (and (not (= i -1))
                  (cond
                    [(char<=? #\0 (string-ref s i) #\9) (s1 (- i 1))]
                    [else #f])))
           (define (s1 i)
             (and (not (= i -1))
                  (let ([c (string-ref s i)])
                    (cond
                      [(char<=? #\1 (string-ref s i) #\9) (s1 (- i 1))]
                      [(char=? c #\.) #t]
                      [(char=? c #\0) (s2 (- i 1))]
                      [else #f]))))
           (define (s2 i)
             (and (not (= i -1))
                  (let ([c (string-ref s i)])
                    (cond
                      [(char<=? #\1 (string-ref s i) #\9) (s1 (- i 1))]
                      [(char=? c #\0) (s2 (- i 1))]
                      [else #f]))))
           (s0 (- n 1))))))
(define label?
  (lambda (x)
    (and (symbol? x)
         (let* ([s (symbol->string x)] [n (string-length s)])
           (define (s0 i)
             (and (not (= i -1))
                  (cond
                    [(char<=? #\0 (string-ref s i) #\9) (s1 (- i 1))]
                    [else #f])))
           (define (s1 i)
             (and (not (= i -1))
                  (let ([c (string-ref s i)])
                    (cond
                      [(char<=? #\1 (string-ref s i) #\9) (s1 (- i 1))]
                      [(char=? c #\$) #t]
                      [(char=? c #\0) (s2 (- i 1))]
                      [else #f]))))
           (define (s2 i)
             (and (not (= i -1))
                  (let ([c (string-ref s i)])
                    (cond
                      [(char<=? #\1 (string-ref s i) #\9) (s1 (- i 1))]
                      [(char=? c #\0) (s2 (- i 1))]
                      [else #f]))))
           (s0 (- n 1))))))
(define prims+
  (append
   '(make-procedure
     procedure-set!
     procedure-code
     procedure-ref)
   prims))
(define prim+?
  (make-predicate prims+))
(define (:begin exp+ k)
  (let-values (((exp* |(exp)|) (split-at-right exp+ 1)))
    (k exp* (car |(exp)|))))
(define (remove-all x lst)
  (remove* (list x) lst))
(define pred-prims
  '(=
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
    eq?))
(define pred-prim?
  (make-predicate pred-prims))
(define value-prims
  '(+
    -
    *
    cons
    car
    cdr
    make-vector
    vector-length
    vector-ref
    box
    unbox
    void
    make-procedure
    procedure-code
    procedure-ref))
(define value-prim?
  (make-predicate value-prims))
(define effect-prims
  '(set-car!
    set-cdr!
    vector-set!
    set-box!
    procedure-set!))
(define effect-prim?
  (make-predicate effect-prims))
;arithmetic-shift(-left)
(define ash arithmetic-shift)
;shift-right-arithmetic
(define (sra n m)
  (arithmetic-shift n (- m)))
(define logand bitwise-and)
(define value-prims-arity
  '((+ . 2)
    (- . 2)
    (* . 2)
    (cons . 2)
    (car . 1)
    (cdr . 1)
    (make-vector . 1)
    (vector-length . 1)
    (vector-ref . 2)
    (box . 1)
    (unbox . 1)
    (void . 0)
    (make-procedure . 2)
    (procedure-code . 1)
    (procedure-ref . 2)))
(define pred-prims-arity
  '((= . 2)
    (< . 2)
    (> . 2)
    (<= . 2)
    (>= . 2)
    (null? . 1)
    (boolean? . 1)
    (fixnum? . 1)
    (pair? . 1)
    (vector? . 1)
    (box? . 1)
    (procedure? . 1)
    (eq? . 2)))
(define effect-prims-arity
  '((set-car! . 2)
    (set-cdr! . 2)
    (vector-set! . 3)
    (set-box! . 2)
    (procedure-set! . 3)))
(define shift-fixnum 3)
(define $false #b00000110)
(define $true  #b00001110)
(define $nil   #b00010110)
(define $void  #b00011110)
(define tag:fixnum    #b000)
(define tag:pair      #b001)
(define tag:procedure #b010)
(define tag:box       #b011)
(define tag:vector    #b111)
(define offset:car 0)
(define offset:cdr 8)
(define size:pair 16)
(define offset:vector-length 0)
(define offset:vector-data 8)
(define size:box 8)
(define offset:box 0)
(define offset:procedure-code 0)
(define offset:procedure-data 8)
(define mask:fixnum    #b111)
(define mask:pair      #b111)
(define mask:vector    #b111)
(define mask:procedure #b111)
(define mask:boolean   #b11110111)
(define tag:boolean    #b00000110)
(define mask:box       #b111)
(define binop?
  (make-predicate '(+ - * sra logand)))
(define relop?
  (make-predicate '(= < > <= >=)))
(define make-proc*
  (case-lambda
    ((proc union)
     (define (proc* exp* k)
       (if (null? exp*)
           (k '() '())
           (proc
            (car exp*)
            (lambda (exp a*)
              (proc*
               (cdr exp*)
               (lambda (exp* b*)
                 (k (cons exp exp*)
                    (union a* b*))))))))
     proc*)
    ((proc) (make-proc* proc U))))
(define (make-begin effect* exp)
  (define (Seq exp)
    (match exp
      ((begin . ,exp*) exp*)
      (,else (list exp))))
  (define exp+
    (append-map Seq `(,@effect* ,exp)))
  (if (null? (cdr exp+))
      (car exp+)
      `(begin . ,exp+)))
(define parameter-registers '(r8 r9))
(define frame-pointer-register 'rbp)
(define return-value-register 'rax)
(define return-address-register 'r15)
(define allocation-pointer-register 'rdx)
(define (make-fv index)
  (string->symbol
   (format "fv~a" index)))