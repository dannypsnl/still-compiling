#lang nanopass

(define-syntax (unimplemented _)
  #'(raise "unimplement"))

(define (op? sym)
  (member sym '(+ - * / = >= > <= <)))

(define-language IR
  (terminals
   (symbol (name))
   (op (op))
   (integer (int)))
  (Program (prog)
           (p inst* ...))
  (Inst (inst)
        (assign name expr) => (name := expr)
        (assign-op name op expr0 expr1) => (name := expr0 op expr1)
        (condbr cond int) => (if cond jump-to int)
        (br int) => (jump-to int))
  (Expr (expr cond)
        name
        int))

(define-pass get-leader* : IR (prog) -> * ()
  (definitions
    ; init with 0 since the first instruction is leader
    (define leader* '(0)))
  (Prog : Program (prog) -> * ()
        [(p ,inst* ...)
         (for ([inst inst*]
               [n (length inst*)])
           (update-leader inst n))
         (reverse leader*)])
  ; * the instruction after jump instruction is leader
  ; * the instruction of jump target is leader
  (update-leader : Inst (inst n) -> * ()
                 [(condbr ,cond ,int)
                  (set! leader* (cons (+ n 1) leader*))
                  (set! leader* (cons int leader*))]
                 [(br ,int)
                  (set! leader* (cons (+ n 1) leader*))
                  (set! leader* (cons int leader*))]
                 [else
                  (void)])
  (Prog prog))

; IR-BB add BasicBlock and convert program from inst* to basic-block*
(define-language IR-BB
  (extends IR)
  (Program (prog)
           (- (p inst* ...))
           (+ (p bb* ...)))
  (BasicBlock (bb)
              (+ (block inst* ...))))

(define-pass IR->IR-BB : IR (prog leader*) -> IR-BB ()
  (definitions
    (define bb* '())
    (define cur-block '()))
  (convert : Program (prog) -> Program ()
           [(p ,inst* ...)
            (for ([inst inst*]
                  [n (length inst*)])
              (define cur-inst (list (inst-IR->BBIR inst)))
              (if (member n leader*)
                  (let ()
                    (unless (empty? cur-block)
                      (set! bb* (append bb* (list cur-block))))
                    (set! cur-block cur-inst))
                  (set! cur-block (append cur-block cur-inst))))
            (set! bb* (append bb* (list cur-block)))
            `(p ,(map inst*->block bb*) ...)])
  (inst*->block : * (inst*) -> BasicBlock ()
                `(block ,inst* ...))
  (inst-IR->BBIR : Inst (inst) -> Inst ())
  (convert prog))

(define-pass liveness-map : IR-BB (bb) -> * ()
  (basic-block : BasicBlock (bb) -> * ()
               [(block ,inst* ...)
                (define mark* (make-hash))
                (define symbol* (make-hash))
                (for ([inst (reverse inst*)])
                  (mark-inst inst mark* symbol*))
                mark*])
  (mark-inst : Inst (inst mark* symbol*) -> * ()
             [(assign ,name ,expr)
              (hash-set! mark* inst (hash-copy symbol*))
              (hash-set! symbol* name '(inactive dead))
              (mark-use expr symbol*)]
             [(assign-op ,name ,op ,expr0 ,expr1)
              (hash-set! mark* inst (hash-copy symbol*))
              (hash-set! symbol* name '(inactive dead))
              (mark-use expr0 symbol*)
              (mark-use expr1 symbol*)]
             [else (void)])
  (mark-use : Expr (expr symbol*) -> * ()
            [,name
             (hash-set! symbol* name 'active)]
            [else (void)])
  (basic-block bb))

(struct node (name node)
  #:transparent)
(define-pass basic-block->DAG : IR-BB (bb) -> * ()
  (basic-block : BasicBlock (bb) -> * ()
               [(block ,inst* ...)
                (define name=>graph (make-hash))
                (map (λ (inst)
                       (inst->node inst name=>graph))
                     inst*)])
  (inst->node : Inst (inst name=>graph) -> * ()
              [(assign ,name ,expr)
               (define g (node name (expr->node expr name=>graph)))
               (hash-set! name=>graph name g)
               g]
              [(assign-op ,name ,op ,expr0 ,expr1)
               (define g (node name `(,op ,(expr->node expr0 name=>graph) ,(expr->node expr1 name=>graph))))
               (hash-set! name=>graph name g)
               g]
              [(condbr ,cond ,int)
               `(condbr ,(expr->node cond name=>graph) ,int)]
              [(br ,int)
               inst])
  (expr->node : Expr (expr name=>graph) -> * ()
              [,int int]
              [,name (if (hash-ref name=>graph name #f)
                         (hash-ref name=>graph name)
                         (let ([val (gensym name)])
                           (hash-set! name=>graph name val)
                           val))])
  (basic-block bb))

(define-pass foreach-block : IR-BB (prog) -> * ()
  (Prog : Program (prog) -> * ()
        [(p ,bb* ...)
         (define inst=>liveness (map liveness-map bb*))
         (for/list ([bb bb*])
           (basic-block->DAG bb))])
  (Prog prog))

(define (all prog)
  (define-parser parse IR)
  (define p (parse prog))
  (define leader* (get-leader* p))
  (define ir-with-bb (IR->IR-BB p leader*))
  (foreach-block ir-with-bb))

#;(all '(p (assign a 1) ;0
           (assign b 2) ;1
           (assign-op c + a b) ;2
           (assign-op d = c 3) ;3
           (condbr d 6) ;4, jump to 6 is c=3
           (assign c a) ;5
           (assign c b) ;6
           ))

(define-pass simplify : (IR-BB Inst) (inst) -> (IR-BB Inst) ()
  (simplify-inst : Inst (inst) -> Inst ()
                 [(assign-op ,name ,op ,expr0 ,expr1)
                  (match* {op expr0 expr1}
                    [{+ x 0} `(assign ,name ,x)]
                    [{+ 0 x} `(assign ,name ,x)]
                    [{- x 0} `(assign ,name ,x)]
                    [{* x 1} `(assign ,name ,x)]
                    [{* 1 x} `(assign ,name ,x)]
                    [{/ x 1} `(assign ,name ,x)]
                    [{_ _ _} inst])])
  (simplify-inst inst))

(module+ test
  (require rackunit)

  (with-output-language (IR-BB Inst)
    (check-equal? (simplify `(assign-op a + x 0))
                  `(assign a x))
    (check-equal? (simplify `(assign-op a + 0 x))
                  `(assign a x))
    (check-equal? (simplify `(assign-op a - x 0))
                  `(assign a x))
    (check-equal? (simplify `(assign-op a * x 1))
                  `(assign a x))
    (check-equal? (simplify `(assign-op a * 1 x))
                  `(assign a x))
    (check-equal? (simplify `(assign-op a / x 1))
                  `(assign a x))))
