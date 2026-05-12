; TEST-ARGS: -disable-undef-input -disable-poison-input
; SKIP-IDENTITY
target triple = "riscv64-unknown-linux-gnu"
target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"

declare i64 @llvm.ctlz.i64(i64, i1)
declare i64 @llvm.bswap.i64(i64)
declare i64 @llvm.smax.i64(i64, i64)

; ---- clz ----
define i64 @src(i64 %a) {
  %r = call i64 @llvm.ctlz.i64(i64 %a, i1 false)
  ret i64 %r
}

define i64 @tgt(i64 %a) {
  %r = call i64 asm "clz $0, $1", "=r,r"(i64 %a)
  ret i64 %r
}

; ---- rev16 ----
define i64 @src_rev16(i64 %a) {
  %r = call i64 @llvm.bswap.i64(i64 %a)
  ret i64 %r
}

define i64 @tgt_rev16(i64 %a) {
  %r = call i64 asm "rev8 $0, $1", "=r,r"(i64 %a)
  ret i64 %r
}

; ---- max_min ----
define i64 @src_max_min(i64 %a, i64 %b) {
  %r = call i64 @llvm.smax.i64(i64 %a, i64 %b)
  ret i64 %r
}

define i64 @tgt_max_min(i64 %a, i64 %b) {
  %r = call i64 asm "max $0, $1, $2", "=r,r,r"(i64 %a, i64 %b)
  ret i64 %r
}
