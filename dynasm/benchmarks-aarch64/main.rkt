#lang racket/base

;;; Main Benchmark Runner
;;; Run all benchmarks with: racket benchmarks/main.rkt

(require "number.rkt"
         "array.rkt"
         "simd.rkt")

(unless (eq? (system-type 'arch) 'aarch64)
  (printf "Skipping aarch64 tests: system architecture is ~a\n" (system-type 'arch))
  (exit 0))

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
