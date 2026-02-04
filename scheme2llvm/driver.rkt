#lang racket
(require "llvm.rkt"
         "compiler.rkt")

(module+ main
  (define debug-pass (make-parameter #f))
  (define output (make-parameter #f))
  (command-line
   #:program "ss"
   #:usage-help "usage: racket driver.rkt [--debug <pass>] <file>"
   #:once-each
   [("--debug") pass "Print result after pass (desugar, beta-reduce, constant-propagate, eliminate-deadcode, closure-conversion, closure-call, etc.)"
                (debug-pass (string->symbol pass))]
   [("-o" "--output") out "Output executable"
                      (output out)]
   #:args (input-file)
   (when (debug-pass)
     (debug-after (debug-pass)))
   (define bc-path (path-replace-extension input-file ".bc"))
   (define forms (with-input-from-file input-file
                   (lambda ()
                     (let loop ([acc '()])
                       (define e (read))
                       (if (eof-object? e)
                           (reverse acc)
                           (loop (cons e acc)))))))
   (compile-to-bitcode `(begin ,@forms) (path->string bc-path))
   (define succ?
     (cond
       [(output)
        (system* (find-executable-path "clang") (path->string bc-path) "runtime.o" "-o" output)]
       [else
        (system* (find-executable-path "clang") (path->string bc-path) "runtime.o")]))
   (unless succ?
     (error 'compile "compile failed"))))
