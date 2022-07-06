#lang curly-fn nanopass

(define (primitive? x)
  (member x '(+ - * /)))
(define-language CL
  (terminals
   (primitive (p))
   (symbol (x))
   (number (n)))
  (Expr (e c body)
        (with-cont e c)
        x ; var
        n ; int
        p ; primitive
        (lambda (x* ...) body) ; lambda
        (let ([x* e*] ...) body)
        (set! x e)
        (if e0 e1 e2)
        (begin e* ...)
        ; app
        (e e* ...)))

(define-pass remove-let : CL (e) -> CL ()
  (T : Expr (e) -> Expr ()
     [(let ([,x* ,[e*]] ...) ,[body])
      `((lambda (,x* ...) ,body)
        ,e* ...)]))

(define-pass wrap-cps-conversion : CL (e) -> CL ()
  (T : Expr (e) -> Expr ()
     [else `(with-cont ,e (lambda (r) (%halt r)))]))

(define-pass cps-conversion : CL (e) -> CL ()
  (T : Expr (e) -> Expr ()
     [(with-cont ,x ,c) `(,c ,x)]
     [(with-cont ,n ,c) `(,c ,n)]
     [(with-cont (set! ,x ,e) ,c)
      `(with-cont ,e
         (lambda (r1)
           (,c (set! ,x r1))))]
     [(with-cont (if ,e0 ,e1 ,e2) ,c)
      `(with-cont ,e0
         (lambda (r1)
           (if r1
               (with-cont ,e1 ,c)
               (with-cont ,e2 ,c))))]
     [(with-cont (begin ,e) ,c) `(,c ,e)]
     [(with-cont (begin ,e* ...) ,c)
      `(with-cont ,(car e*)
         (lambda (r1)
           (with-cont (begin ,(cdr e*) ...) ,c)))]
     [(with-cont (lambda (,x* ...) ,body) ,c)
      `(,c (lambda (,x* ... k)
             (with-cont ,body k)))]
     [(with-cont (,p ,e* ...) ,c)
      (define r* (map (λ (e) (gensym 'r.)) e*))
      (foldr (λ (e r acc)
               `(with-cont ,e
                  (lambda (,r)
                    ,acc)))
             `(,c (,p ,r* ...))
             e*
             r*)]
     [(with-cont (,e ,e* ...) ,c)
      (define r (gensym 'r.))
      (define r* (map (λ (_) (gensym 'r.)) e*))
      (foldr (λ (e r acc)
               (println acc)
               `(with-cont ,e
                  (lambda (,r)
                    ,acc)))
             `(,r ,r* ... ,c)
             (cons e e*)
             (cons r r*))]
     [(with-cont ((lambda (,x* ...) ,body) ,e* ...) ,c)
      (foldr (λ (x e acc)
               `(with-cont ,e
                  (lambda (,x)
                    ,acc)))
             `(with-cont ,body ,c)
             x*
             e*)])
  (let loop ([e-s e])
    (define e-t (T e-s))
    (if (equal? e-t e-s)
        e-t
        (loop e-t))))

(define-language L
  (extends CL)
  (Expr (e body)
        (- (with-cont e c))))
(define-pass ensure-cps-done : CL (e) -> L ()
  (T : Expr (e) -> Expr ()))

(define-parser parse-CL CL)
(define T (compose ensure-cps-done
                   cps-conversion
                   wrap-cps-conversion
                   remove-let
                   parse-CL))
(define (%halt x) x)

(T '1)
(T 'a)
(T '(let ([mult (lambda (a b) (* a b))])
      (let ([square (lambda (x) (mult x x))])
        (+ (square 10) 1))))

(define call/cc
  (lambda (f k)
    (f k (lambda (result dummy-k)
           (k result)))))
