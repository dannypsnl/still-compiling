#lang racket/base

(require "../dynasm.rkt"
         ffi/unsafe
         racket/file)

;; Threshold: number of loop iterations before triggering compilation
(define THRESHOLD 100)

(define SVC-0x80 #xD4001001)

;; Instruction counts per BF operation
(define (bf-instr-count ch)
  (case ch
    [(#\>) 1]
    [(#\<) 1]
    [(#\+) 3]
    [(#\-) 3]
    [(#\.) 5]
    [(#\,) 5]
    [(#\[) 3]
    [(#\]) 3]
    [else #f]))

;; Scan forward from pc to find matching ']'
(define (scan-forward ops pc)
  (let loop ([i (add1 pc)] [depth 1])
    (case (vector-ref ops i)
      [(#\[) (loop (add1 i) (add1 depth))]
      [(#\]) (if (= depth 1) i (loop (add1 i) (sub1 depth)))]
      [else (loop (add1 i) depth)])))

;; Scan backward from pc to find matching '['
(define (scan-backward ops pc)
  (let loop ([i (sub1 pc)] [depth 1])
    (case (vector-ref ops i)
      [(#\]) (loop (sub1 i) (add1 depth))]
      [(#\[) (if (= depth 1) i (loop (sub1 i) (sub1 depth)))]
      [else (loop (sub1 i) depth)])))

(struct compiled-frame (fn buf))

;; Compile a single loop from loop-start-pc ([) to loop-end-pc (]) inclusive.
;; Function signature: (_fun _pointer -> _pointer)
;;   x0 = mem + dp (pointer to current cell)
;;   returns updated pointer after loop completes
(define (compile-loop ops loop-start-pc loop-end-pc)
  (define range-len (add1 (- loop-end-pc loop-start-pc)))

  ;; Compute instruction positions relative to the loop range
  (define positions (make-vector (add1 range-len) 0))
  (for ([i (in-range range-len)])
    (vector-set! positions (add1 i)
                 (+ (vector-ref positions i)
                    (bf-instr-count (vector-ref ops (+ loop-start-pc i))))))

  ;; Match brackets within the range
  (define match-bracket (make-vector range-len 0))
  (define stack '())
  (for ([i (in-range range-len)])
    (case (vector-ref ops (+ loop-start-pc i))
      [(#\[)
       (set! stack (cons i stack))]
      [(#\])
       (define open (car stack))
       (set! stack (cdr stack))
       (vector-set! match-bracket open i)
       (vector-set! match-bracket i open)]
      [else (void)]))

  ;; Prologue: stp x29,x30,[sp,#-32]! ; mov x29,sp ; str x19,[x29,#16] ; mov x19,x0
  (define prologue-size 4)
  ;; Epilogue: mov x0,x19 ; ldr x19,[x29,#16] ; ldp x29,x30,[sp],#32 ; ret
  (define epilogue-size 4)
  (define total-instrs (+ prologue-size (vector-ref positions range-len) epilogue-size))
  (define buf (dynasm-create (* (+ total-instrs 16) 4)))

  ;; === Prologue ===
  (emit! buf (aarch64-stp-pre X29 X30 SP -32))
  (emit! buf (aarch64-add-imm X29 SP 0))        ; mov x29, sp
  (emit! buf (aarch64-str-imm X19 X29 2))        ; str x19, [x29, #16]
  (emit! buf (aarch64-add-imm X19 X0 0))         ; mov x19, x0 (data pointer)

  ;; === BF ops for loop-start-pc..loop-end-pc ===
  (for ([i (in-range range-len)])
    (define op (vector-ref ops (+ loop-start-pc i)))
    (case op
      [(#\>)
       (emit! buf (aarch64-add-imm X19 X19 1))]
      [(#\<)
       (emit! buf (aarch64-sub-imm X19 X19 1))]
      [(#\+)
       (emit! buf (aarch64-ldrb-imm X0 X19 0))
       (emit! buf (aarch64-add-imm X0 X0 1))
       (emit! buf (aarch64-strb-imm X0 X19 0))]
      [(#\-)
       (emit! buf (aarch64-ldrb-imm X0 X19 0))
       (emit! buf (aarch64-sub-imm X0 X0 1))
       (emit! buf (aarch64-strb-imm X0 X19 0))]
      [(#\.)
       (emit! buf (aarch64-movz X0 1 0))
       (emit! buf (aarch64-add-imm X1 X19 0))
       (emit! buf (aarch64-movz X2 1 0))
       (emit! buf (aarch64-movz X16 4 0))
       (emit! buf SVC-0x80)]
      [(#\,)
       (emit! buf (aarch64-movz X0 0 0))
       (emit! buf (aarch64-add-imm X1 X19 0))
       (emit! buf (aarch64-movz X2 1 0))
       (emit! buf (aarch64-movz X16 3 0))
       (emit! buf SVC-0x80)]
      [(#\[)
       (emit! buf (aarch64-ldrb-imm X0 X19 0))
       (emit! buf (aarch64-cmp-imm X0 0))
       (define close-rel (vector-ref match-bracket i))
       (define here (+ prologue-size (vector-ref positions i) 2))
       (define target (+ prologue-size (vector-ref positions close-rel) 3))
       (emit! buf (aarch64-b-cond COND-EQ (- target here)))]
      [(#\])
       (emit! buf (aarch64-ldrb-imm X0 X19 0))
       (emit! buf (aarch64-cmp-imm X0 0))
       (define open-rel (vector-ref match-bracket i))
       (define here (+ prologue-size (vector-ref positions i) 2))
       (define target (+ prologue-size (vector-ref positions open-rel)))
       (emit! buf (aarch64-b-cond COND-NE (- target here)))]))

  ;; === Epilogue ===
  (emit! buf (aarch64-add-imm X0 X19 0))         ; mov x0, x19 (return updated ptr)
  (emit! buf (aarch64-ldr-imm X19 X29 2))
  (emit! buf (aarch64-ldp-post X29 X30 SP 32))
  (emit! buf (aarch64-ret))

  (define fn (make-jit-function buf (_fun _pointer -> _pointer)))
  (compiled-frame fn buf))

;; Main entry: interpreter + compile hot loops
(define (brainfuck-jit source)
  (define ops
    (for/vector ([ch (in-string source)]
                 #:when (bf-instr-count ch))
      ch))
  (define n (vector-length ops))

  (define mem (malloc 30000 'raw))
  (memset mem 0 30000)

  ;; pc |-> executed times
  (define loop-counting (make-hash))
  ;; pc |-> (compiled-frame fn buf)
  (define compiled-frames (make-hash))

  (let loop ([pc 0] [dp 0])
    (when (< pc n)
      (case (vector-ref ops pc)
        [(#\>) (loop (add1 pc) (add1 dp))]
        [(#\<) (loop (add1 pc) (sub1 dp))]
        [(#\+)
         (define v (ptr-ref mem _byte dp))
         (ptr-set! mem _byte dp (bitwise-and (add1 v) #xFF))
         (loop (add1 pc) dp)]
        [(#\-)
         (define v (ptr-ref mem _byte dp))
         (ptr-set! mem _byte dp (bitwise-and (sub1 v) #xFF))
         (loop (add1 pc) dp)]
        [(#\.)
         (write-char (integer->char (ptr-ref mem _byte dp)))
         (flush-output)
         (loop (add1 pc) dp)]
        [(#\,)
         (define c (read-char))
         (ptr-set! mem _byte dp (if (eof-object? c) 0 (char->integer c)))
         (loop (add1 pc) dp)]
        [(#\[)
         (cond
           [(zero? (ptr-ref mem _byte dp))
            (loop (+ (scan-forward ops pc) 1) dp)]
           [else
            (define count (add1 (hash-ref loop-counting pc 0)))
            (hash-set! loop-counting pc count)
            (cond
              ; Hot loop - compile if needed, then call
              [(>= count THRESHOLD)
               (let* ([entry (hash-ref compiled-frames pc #f)]
                      [entry (or entry
                                 (let ([end-pc (scan-forward ops pc)])
                                   (define e (compile-loop ops pc end-pc))
                                   (hash-set! compiled-frames pc e)
                                   e))]
                      [fn (compiled-frame-fn entry)]
                      [result (fn (ptr-add mem dp))]
                      [new-dp (- (cast result _pointer _intptr)
                                 (cast mem _pointer _intptr))])
                 (loop
                  ; skip past the matching ']'
                  (add1 (scan-forward ops pc))
                  ; continue with new dp
                  new-dp))]
              ; usual path is handled by interpreter
              [else (loop (add1 pc) dp)])])]
        [(#\])
         (cond
           [(not (zero? (ptr-ref mem _byte dp)))
            (loop (scan-backward ops pc) dp)]
           [else (loop (add1 pc) dp)])]
        [else (loop (add1 pc) dp)])))

  ;; Cleanup
  (free mem)
  (for ([entry (in-hash-values compiled-frames)])
    (dynasm-free (compiled-frame-buf entry))))

(module+ main
  (require racket/cmdline)
  (define filename
    (command-line
     #:args (file)
     file))
  (brainfuck-jit (file->string filename)))
