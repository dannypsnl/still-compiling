#lang nanopass

(require graph)

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
        (ret expr))
  (Prog (prog)
        (inst* ...)))

(define-pass live-set* : (IR Prog) (prog) -> * ()
  (definitions
    (define live-set (mutable-set))
    (define (add-live! expr)
      (when (symbol? expr)
        (set-add! live-set expr))))
  (f : Inst (inst) -> * ()
     [(,name ,expr)
      (set-remove! live-set name)
      (add-live! expr)
      (set->list live-set)]
     [(,name ,op ,expr0 ,expr1)
      (set-remove! live-set name)
      (add-live! expr0)
      (add-live! expr1)
      (set->list live-set)]
     [(ret ,expr)
      (add-live! expr)
      (set->list live-set)])
  (Prog : Prog (prog) -> * ()
        [(,inst* ...)
         (map f (reverse inst*))]))

(define (conflict-graph code)
  (define live-sets (live-set* code))
  (define g
    (unweighted-graph/undirected
     (foldl append
            '()
            (map (λ (s) (combinations s 2))
                 live-sets))))
  (for ([s live-sets])
    (match s
      [(list e)
       (add-vertex! g e)]
      [_ (void)]))
  g)

(define reg-list '(r1 r2 r3 r4 r5 r6 r7 r8 r9 r10))
(define (reg? v)
  (member v (cons 'r-ret reg-list)))
(define-language Asm
  (entry Prog)
  (terminals
   (reg (reg))
   (integer (int)))
  (Expr (expr)
        reg
        int)
  (Inst (inst)
        (MOV reg expr)
        (ADD reg expr0 expr1)
        (SUB reg expr0 expr1)
        (MUL reg expr0 expr1)
        (DIV reg expr0 expr1)
        (RET))
  (Prog (prog)
        (inst* ...)))

(define-pass reg/alloc : (IR Prog) (prog) -> (Asm Prog) ()
  (definitions
    (define allocation-map (coloring/brelaz (conflict-graph prog)))
    (define (to-reg expr)
      (define r (hash-ref allocation-map expr #f))
      (if r
          (if (>= r (length reg-list))
              (error 'store/load-global)
              (list-ref reg-list r))
          expr)))
  (f : Inst (inst) -> Inst ()
     [(,name ,expr)
      `(MOV ,(to-reg name) ,(to-reg expr))]
     [(,name ,op ,expr0 ,expr1)
      (case op
        [(+) `(ADD ,(to-reg name) ,(to-reg expr0) ,(to-reg expr1))]
        [(-) `(SUB ,(to-reg name) ,(to-reg expr0) ,(to-reg expr1))]
        [(*) `(MUL ,(to-reg name) ,(to-reg expr0) ,(to-reg expr1))]
        [(/) `(DIV ,(to-reg name) ,(to-reg expr0) ,(to-reg expr1))])]
     [(ret ,expr)
      (list `(MOV r-ret ,(to-reg expr))
            `(RET))])
  (Prog : Prog (prog) -> Prog ()
        [(,inst* ...)
         `(,(flatten (map f inst*)) ...)]))

(module+ test
  (require rackunit)

  (define-parser parse IR)
  (define-parser parse-asm Asm)

  (check-equal? (reg/alloc (parse '([a 1]
                                    [b 2]
                                    [f 3]
                                    [c + a b]
                                    [e - c d]
                                    [g * e c]
                                    [h / f g]
                                    [i h]
                                    [ret i])))
                (parse-asm '((MOV r3 1)
                             (MOV r4 2)
                             (MOV r1 3)
                             (ADD r3 r3 r4)
                             (SUB r2 r3 r2)
                             (MUL r2 r2 r3)
                             (DIV r1 r1 r2)
                             (MOV r1 r1)
                             (MOV r-ret r1)
                             (RET)))))
