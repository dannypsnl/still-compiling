#lang racket

(define (brainfuck program)
  (define mem (make-bytes 30000 0))
  (define src program)
  (define src-len (string-length src))
  (let loop ([pc 0] [dp 0])
    (when (< pc src-len)
      (case (string-ref src pc)
        [(#\>) (loop (add1 pc) (add1 dp))]
        [(#\<) (loop (add1 pc) (sub1 dp))]
        [(#\+)
         (bytes-set! mem dp (bitwise-and (add1 (bytes-ref mem dp)) #xFF))
         (loop (add1 pc) dp)]
        [(#\-)
         (bytes-set! mem dp (bitwise-and (sub1 (bytes-ref mem dp)) #xFF))
         (loop (add1 pc) dp)]
        [(#\.)
         (write-char (integer->char (bytes-ref mem dp)))
         (loop (add1 pc) dp)]
        [(#\[)
         (if (zero? (bytes-ref mem dp))
             (let scan ([j (add1 pc)] [depth 1])
               (case (string-ref src j)
                 [(#\])
                  (if (= depth 1)
                      (loop (add1 j) dp)
                      (scan (add1 j) (sub1 depth)))]
                 [(#\[) (scan (add1 j) (add1 depth))]
                 [else (scan (add1 j) depth)]))
             (loop (add1 pc) dp))]
        [(#\])
         (if (not (zero? (bytes-ref mem dp)))
             (let scan ([j (sub1 pc)] [depth 1])
               (case (string-ref src j)
                 [(#\[)
                  (if (= depth 1)
                      (loop j dp)
                      (scan (sub1 j) (sub1 depth)))]
                 [(#\]) (scan (sub1 j) (add1 depth))]
                 [else (scan (sub1 j) depth)]))
             (loop (add1 pc) dp))]
        [else (loop (add1 pc) dp)]))))

(module+ main
  (command-line
   #:args (file)
   (brainfuck (file->string file))))
