#lang racket
(require "llvm.rkt"
         "compiler.rkt")

(module+ main
  (define debug-pass #f)
  (command-line
   #:program "ss"
   #:usage-help "usage: racket driver.rkt [--debug <pass>] <file>"
   #:once-each
   [("--debug") pass "Print result after pass (desugar, beta-reduce, constant-propagate, eliminate-deadcode, closure-conversion, closure-call, etc.)"
                (set! debug-pass (string->symbol pass))]
   #:args (input-file)
   (when debug-pass
     (debug-after debug-pass))
   (define bc-path (path-replace-extension input-file ".bc"))
   (define out-path (path-replace-extension input-file ""))
   (define forms (with-input-from-file input-file
                   (lambda ()
                     (let loop ([acc '()])
                       (define e (read))
                       (if (eof-object? e)
                           (reverse acc)
                           (loop (cons e acc)))))))
   (compile-to-bitcode `(begin ,@forms) (path->string bc-path))
   (define succ? (system* (find-executable-path "clang") (path->string bc-path) "runtime.o" "-o" (path->string out-path)))
   (unless succ?
     (error 'compile "compile failed"))))
