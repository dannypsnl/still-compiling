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

(define-parser parse IR)

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

(define prog
  (parse '(p (assign a 1) ;0
             (assign b 2) ;1
             (assign-op c + a b) ;2
             (assign-op d = c 3) ;3
             (condbr d 6) ;4, jump to 6 is c=3
             (assign c 0) ;5
             (assign c 1) ;6
             )))
(define leader* (get-leader* prog))

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

(define ir-with-bb (IR->IR-BB prog leader*))

(define-pass liveness-map : IR-BB (prog) -> * ()
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
  (Prog : Program (prog) -> * ()
        [(p ,bb* ...)
         (map basic-block bb*)])
  (Prog prog))

(liveness-map ir-with-bb)
