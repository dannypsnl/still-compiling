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

(define-parser parse IR)
(local-value-numbering
 (parse '([a 1]
          [b 2]
          [c + a b]
          [d - a b]
          [e + a b]
          [f e])))
