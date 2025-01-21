#lang racket
(require (for-syntax syntax/parse))
(require (for-syntax todo))

(define-syntax (return stx)
  (syntax-parse stx
    [(_ e)
      #'(fprintf (current-output-port)
          "return ~a;" #'e)]))

(define-syntax (func stx)
  (syntax-parse stx
    #:datum-literals (: returns)
    [(_ name:id ([param*:id : ty] ...) (returns ret-ty) stmt* ... stmt)
      #'(begin 
          (fprintf (current-output-port)
            "~a ~a ("
            #'ret-ty
            #'name)
          (fprintf (current-output-port) ") {")
          stmt* ... stmt
          (fprintf (current-output-port) "}")
        )]))

(define f (open-output-file "_build/out.c"
            #:mode 'text
            #:exists 'truncate))
(parameterize ([current-output-port f])
  (func main () (returns int)
    (return 0))
  )
