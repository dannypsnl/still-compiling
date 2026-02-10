# dynasm

A small JIT framework in Racket, architecture

- [x] aarch64
- [x] x86/64
- [x] RISC-V

## Dependencies

1. unicorn engine (v2) for testing https://www.unicorn-engine.org/

## Idea

1. JIT 使用 `mmap` 要一塊記憶體 `X`（可寫入）
2. 還是編譯出機器碼，但把編譯出來的機器碼 `memcpy` 進去記憶體 `X`（再修改成可執行 Write XOR Execute policy）
3. 用轉型的方式讓記憶體 `X` 成為 C 函數並調用

## References

- https://github.com/hellerve-pl-experiments/cj
- https://nickdesaulniers.github.io/blog/2013/04/03/basic-jit/
- https://github.com/spencertipping/jit-tutorial
- https://github.com/spencertipping/canard/blob/circular/bin/canard.md#introduction
