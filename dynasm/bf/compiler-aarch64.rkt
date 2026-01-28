#lang racket/base

;;; Brainfuck compiler using AArch64 dynasm
;;; Uses macOS syscalls (svc #0x80) for I/O instead of C function calls.

(require "../dynasm.rkt"
         ffi/unsafe
         racket/file)

;; macOS AArch64 syscall: svc #0x80, syscall number in x16
;; write = 4 (x0=fd, x1=buf, x2=count)
;; read  = 3 (x0=fd, x1=buf, x2=count)
(define SVC-0x80 #xD4001001)

;; Instruction counts per BF operation
(define (bf-instr-count ch)
  (case ch
    [(#\>) 1]
    [(#\<) 1]
    [(#\+) 3]
    [(#\-) 3]
    [(#\.) 5]  ; mov x0,#1; mov x1,x19; mov x2,#1; mov x16,#4; svc
    [(#\,) 5]  ; mov x0,#0; mov x1,x19; mov x2,#1; mov x16,#3; svc
    [(#\[) 3]
    [(#\]) 3]
    [else #f]))

(define (compile-bf source)
  ;; Filter to BF characters only
  (define ops
    (for/vector ([ch (in-string source)]
                 #:when (bf-instr-count ch))
      ch))
  (define n (vector-length ops))

  ;; Pass 1: instruction positions
  (define positions (make-vector (+ n 1) 0))
  (for ([i (in-range n)])
    (vector-set! positions (+ i 1)
                 (+ (vector-ref positions i)
                    (bf-instr-count (vector-ref ops i)))))

  ;; Match brackets
  (define match-bracket (make-vector n 0))
  (define stack '())
  (for ([i (in-range n)])
    (case (vector-ref ops i)
      [(#\[) (set! stack (cons i stack))]
      [(#\])
       (when (null? stack) (error 'compile-bf "unmatched ]"))
       (define open (car stack))
       (set! stack (cdr stack))
       (vector-set! match-bracket open i)
       (vector-set! match-bracket i open)]
      [else (void)]))
  (when (pair? stack) (error 'compile-bf "unmatched ["))

  ;; Prologue: 4 instructions
  ;; stp x29, x30, [sp, #-32]!
  ;; mov x29, sp
  ;; str x19, [x29, #16]       (scaled offset 2)
  ;; mov x19, x0               (data pointer = first arg)
  ;;
  ;; Epilogue: 3 instructions
  ;; ldr x19, [x29, #16]
  ;; ldp x29, x30, [sp], #32
  ;; ret

  (define prologue-size 4)
  (define epilogue-size 3)
  (define total-instrs (+ prologue-size (vector-ref positions n) epilogue-size))
  (define buf (dynasm-create (* (+ total-instrs 16) 4)))

  ;; === Prologue ===
  (emit! buf (aarch64-stp-pre X29 X30 SP -32))
  (emit! buf (aarch64-add-imm X29 SP 0))        ; mov x29, sp
  (emit! buf (aarch64-str-imm X19 X29 2))       ; str x19, [x29, #16]
  (emit! buf (aarch64-add-imm X19 X0 0))        ; mov x19, x0

  ;; === BF ops ===
  (for ([i (in-range n)])
    (case (vector-ref ops i)
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
       ;; write(1, x19, 1) via syscall
       (emit! buf (aarch64-movz X0 1 0))        ; fd = stdout
       (emit! buf (aarch64-add-imm X1 X19 0))   ; buf = data ptr
       (emit! buf (aarch64-movz X2 1 0))         ; count = 1
       (emit! buf (aarch64-movz X16 4 0))        ; syscall = write
       (emit! buf SVC-0x80)]
      [(#\,)
       ;; read(0, x19, 1) via syscall
       (emit! buf (aarch64-movz X0 0 0))         ; fd = stdin
       (emit! buf (aarch64-add-imm X1 X19 0))   ; buf = data ptr
       (emit! buf (aarch64-movz X2 1 0))         ; count = 1
       (emit! buf (aarch64-movz X16 3 0))        ; syscall = read
       (emit! buf SVC-0x80)]
      [(#\[)
       (emit! buf (aarch64-ldrb-imm X0 X19 0))
       (emit! buf (aarch64-cmp-imm X0 0))
       (define close-idx (vector-ref match-bracket i))
       (define here (+ prologue-size (vector-ref positions i) 2))
       (define target (+ prologue-size (vector-ref positions close-idx) 3))
       (emit! buf (aarch64-b-cond COND-EQ (- target here)))]
      [(#\])
       (emit! buf (aarch64-ldrb-imm X0 X19 0))
       (emit! buf (aarch64-cmp-imm X0 0))
       (define open-idx (vector-ref match-bracket i))
       (define here (+ prologue-size (vector-ref positions i) 2))
       (define target (+ prologue-size (vector-ref positions open-idx)))
       (emit! buf (aarch64-b-cond COND-NE (- target here)))]))

  ;; === Epilogue ===
  (emit! buf (aarch64-ldr-imm X19 X29 2))       ; ldr x19, [x29, #16]
  (emit! buf (aarch64-ldp-post X29 X30 SP 32))
  (emit! buf (aarch64-ret))

  (define fn (make-jit-function buf (_fun _pointer -> _void)))
  (values fn buf))

(define (brainfuck-compiler source)
  (define mem (malloc 30000 'raw))
  (memset mem 0 30000)
  (define-values (fn buf) (compile-bf source))
  (fn mem)
  (free mem)
  (dynasm-free buf))

(module+ main
  (require racket/cmdline)
  (define filename
    (command-line
     #:args (file)
     file))
  (brainfuck-compiler (file->string filename)))
