; libc memset returns its destination argument, so a function that tail-calls
; it and returns that pointer may leave the result in a0. We model memset with
; llvm.memset, which returns void, so a0 has to be installed by hand.

target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

declare void @llvm.memset.p0.i64(ptr writeonly captures(none), i8, i64, i1 immarg)

define ptr @memset_returns_dest(ptr %p, i64 %n) {
  tail call void @llvm.memset.p0.i64(ptr align 4 %p, i8 0, i64 %n, i1 false)
  ret ptr %p
}
