#lang info
(define collection "still-compiling")
(define deps '("base"
               ; lang extension
               "curly-fn-lib"
               ; compiler
               "nanopass"
               "graph"
               ; parser combinator
               "megaparsack-lib"
               "megaparsack-parser-tools"
               "functional-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/still-compiling.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(dannypsnl))
