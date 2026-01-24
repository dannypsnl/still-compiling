#lang racket/base

;;; Main Benchmark Runner
;;; Run all benchmarks with: racket benchmarks/main.rkt

(require "number.rkt"
         "array.rkt"
         "simd.rkt")

(printf "========================================\n")
(printf "       DYNASM BENCHMARK SUITE\n")
(printf "========================================\n")
(printf "\nRunning all benchmarks...\n")

;; Run all benchmark suites
(run-numeric-benchmarks)
(run-array-benchmarks)
(run-simd-benchmarks)

(printf "\n========================================\n")
(printf "       ALL BENCHMARKS COMPLETE\n")
(printf "========================================\n")
