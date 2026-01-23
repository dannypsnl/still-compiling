#lang racket/base

;;; Common utilities for benchmarks

(require ffi/unsafe
         ffi/vector
         "../dynasm.rkt")

(provide (all-defined-out)
         (all-from-out "../dynasm.rkt")
         (all-from-out ffi/unsafe)
         (all-from-out ffi/vector))

;; Formatting helper
(define (~r n #:precision [p 2])
  (real->decimal-string n p))

;; Memory allocation
(define posix-memalign
  (get-ffi-obj "posix_memalign" (ffi-lib #f)
               (_fun (ptr : (_ptr o _pointer)) _size _size -> (r : _int)
                     -> (if (zero? r) ptr (error "posix_memalign failed")))))

(define libc-free
  (get-ffi-obj "free" (ffi-lib #f) (_fun _pointer -> _void)))

(define (aligned-malloc size)
  (posix-memalign 16 size))

;; Standard benchmark runner
(define (benchmark name iterations jit-fn racket-fn . args)
  (printf "\n~a (~a iterations):\n" name iterations)

  ;; Warmup
  (for ([_ (in-range 1000)])
    (apply jit-fn args)
    (apply racket-fn args))

  ;; JIT benchmark
  (collect-garbage)
  (collect-garbage)
  (define jit-start (current-inexact-milliseconds))
  (for ([_ (in-range iterations)])
    (apply jit-fn args))
  (define jit-time (- (current-inexact-milliseconds) jit-start))

  ;; Racket benchmark
  (collect-garbage)
  (collect-garbage)
  (define racket-start (current-inexact-milliseconds))
  (for ([_ (in-range iterations)])
    (apply racket-fn args))
  (define racket-time (- (current-inexact-milliseconds) racket-start))

  ;; Results
  (printf "  JIT:    ~a ms\n" (~r jit-time #:precision 2))
  (printf "  Racket: ~a ms\n" (~r racket-time #:precision 2))
  (define speedup (/ racket-time jit-time))
  (printf "  Speedup: ~ax ~a\n"
          (~r speedup #:precision 2)
          (if (> speedup 1) "(JIT faster)" "(Racket faster)")))
