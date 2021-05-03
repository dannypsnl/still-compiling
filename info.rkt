#lang info
(define collection "still-compiling")
(define deps '("base"
               ["nanopass" #:version "6.2.1"]))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/still-compiling.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(dannypsnl))
