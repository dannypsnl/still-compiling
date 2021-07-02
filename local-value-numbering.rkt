#lang nanopass

(define (op? v)
  (member v '(+ - * / ^)))
(define-language IR
  (entry Prog)
  (terminals
   (symbol (name))
   (op (op))
   (integer (int)))
  (Expr (expr)
        name
        int)
  (Inst (inst)
        (name expr) => (name = expr)
        (name op expr0 expr1) => (name = expr0 op expr1)
        )
  (Prog (prog)
        (inst* ...)))

(define-pass local-value-numbering : (IR Prog) (prog) -> (IR Prog) ()
  (definitions
    (define m (make-hash)))
  (f : Inst (inst) -> Inst ()
     [(,name ,expr)
      (hash-set! m name (hash-ref m expr expr))
      `(,name ,(hash-ref m expr expr))]
     [(,name ,op ,expr0 ,expr1)
      (define v0 (hash-ref m expr0))
      (define v1 (hash-ref m expr1))
      (define new-key (list op v0 v1))
      (if (hash-ref m new-key #f)
          (let ([v (hash-ref m new-key)])
            (hash-set! m name v)
            `(,name ,v))
          (begin
            (hash-set! m new-key name)
            inst))])
  (p : Prog (prog) -> Prog ()
     [(,inst* ...)
      `(,(map f inst*) ...)])
  (p prog))

(define-pass extend-local-value-numbering : (IR Prog) (prog) -> (IR Prog) ()
  (definitions
    (define m (make-hash))
    (define const-value (make-hash))
    (define (const? v)
      (hash-ref const-value v
                (integer? v))))
  (f : Inst (inst) -> Inst ()
     [(,name ,expr)
      (if (const? expr)
          (begin
            (hash-set! const-value name
                       (hash-ref const-value expr expr))
            `(,name ,(hash-ref const-value name)))
          (begin
            (hash-set! m name (hash-ref m expr expr))
            `(,name ,(hash-ref m expr expr))))]
     [(,name ,op ,expr0 ,expr1)
      (define v0 (hash-ref m expr0
                           (hash-ref const-value expr0 expr0)))
      (define v1 (hash-ref m expr1
                           (hash-ref const-value expr1 expr1)))
      (if (and (const? expr0) (const? expr1))
          (begin
            (hash-set! const-value name
                       (case op
                         [(+) (+ v0 v1)]
                         [(-) (- v0 v1)]
                         [(*) (* v0 v1)]
                         [(/) (/ v0 v1)]))
            `(,name ,(hash-ref const-value name)))
          (match {list op v0 v1}
            [(or (list '+ a 0)
                 (list '+ 0 a))
             `(,name ,a)]
            [(list '- a a)
             (hash-set! const-value name 0)
             `(,name 0)]
            [(list '- a 0) `(,name ,a)]
            [(or (list '* a 2)
                 (list '* 2 a))
             `(,name + ,a ,a)]
            [(or (list '* a 1)
                 (list '* 1 a))
             `(,name ,a)]
            [(or (list '* a 0)
                 (list '* 0 a))
             (hash-set! const-value name 0)
             `(,name 0)]
            [(list '/ a 1)
             `(,name ,a)]
            [(list '/ a a) #:when (not (eq? a 0))
                           (hash-set! const-value name 1)
                           `(,name 1)]
            [_ (define new-key (set op v0 v1))
               (if (hash-ref m new-key #f)
                   (let ([v (hash-ref m new-key)])
                     (hash-set! m name v)
                     `(,name ,v))
                   (begin
                     (hash-set! m new-key name)
                     inst))]))])
  (p : Prog (prog) -> Prog ()
     [(,inst* ...)
      `(,(map f inst*) ...)])
  (p prog))

(module+ test
  (require rackunit)

  (define-parser parse IR)

  (check-equal? (local-value-numbering
                 (parse '([a 1]
                          [b 2]
                          [c + a b]
                          [d - a b]
                          [e + a b]
                          [f e]
                          [g + b a])))
                (parse '([a 1]
                         [b 2]
                         [c + a b]
                         [d - a b]
                         [e c]
                         [f c]
                         [g + b a])))

  (test-case "constant folding"
             (check-equal? (extend-local-value-numbering
                            (parse '([a 1]
                                     [b 2]
                                     [c + a b]
                                     [d - a b]
                                     [e + a b]
                                     [f e]
                                     [g + b a])))
                           (parse '([a 1]
                                    [b 2]
                                    [c 3]
                                    [d -1]
                                    [e 3]
                                    [f 3]
                                    [g 3]))))

  (test-case "commutative operations"
             (check-equal? (extend-local-value-numbering
                            (parse '([c + a b]
                                     [d - a b]
                                     [e + a b]
                                     [f e]
                                     [g + b a])))
                           (parse '([c + a b]
                                    [d - a b]
                                    [e c]
                                    [f c]
                                    [g c]))))

  (test-case "algebraic identities"
             (check-equal? (extend-local-value-numbering
                            (parse '([c + 0 a]
                                     [d - a 0]
                                     [e * a 2]
                                     [f - b b]
                                     [g / b b])))
                           (parse '([c a]
                                    [d a]
                                    [e + a a]
                                    [f 0]
                                    [g 1])))))
