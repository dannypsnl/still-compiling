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
        (ret expr)
        )
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
  (p : Prog (prog) -> * ()
     [(,inst* ...)
      (map f (reverse inst*))])
  (p prog))

(define (conflict-graph code)
  (define live-sets (live-set* code))
  (unweighted-graph/undirected
   (foldl append
          '()
          (map (λ (s) (combinations s 2))
               live-sets))))

(define-parser parse IR)

(define g
  (conflict-graph (parse '([c + a b]
                           [e - c d]
                           [e * e c]
                           [f / f e]
                           [ret f]))))

(coloring/brelaz g)
