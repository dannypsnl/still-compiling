#lang racket
(require "llvm.rkt")

(module+ main
  (command-line
   #:program "ss"
   #:usage-help "usage: racket driver.rkt <file> -o <output>"

   #:args (input-file)
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
