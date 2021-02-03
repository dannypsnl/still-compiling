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
        (assign name expr)
        (condbr cond int)
        (br int))
  (Expr (expr cond)
        name
        int
        ; only binary operator here
        (op expr0 expr1)))

(define-parser parse IR)

(define-pass get-leader* : IR (prog) -> * ()
  (definitions
    ; init with 0 => #t, since the first instruction is leader
    (define leader-map (make-hash '((0 . #t)))))
  (Prog : Program (prog) -> * ()
        [(p ,inst* ...)
         (for ([inst inst*]
               [n (length inst*)])
           (update-leader inst n))
         leader-map])
  ; * the instruction after jump instruction is leader
  ; * the instruction of jump target is leader
  (update-leader : Inst (inst n) -> * ()
                 [(condbr ,cond ,int)
                  (hash-set! leader-map (+ n 1) #t)
                  (hash-set! leader-map int #t)]
                 [(br ,int)
                  (hash-set! leader-map (+ n 1) #t)
                  (hash-set! leader-map int #t)]
                 [else
                  (void)])
  (Prog prog))

(define prog
  (parse '(p (assign a 1) ;0
             (assign b 2) ;1
             (assign c (+ a b)) ;2
             (condbr (= c 3) 5) ;3, jump to 5 is c=3
             (assign c 0) ;4
             (assign c 1) ;5
             )))
(define leader-map (get-leader* prog))

