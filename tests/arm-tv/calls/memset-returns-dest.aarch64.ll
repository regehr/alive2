; libc memset returns its destination argument, so a function that tail-calls
; it and returns that pointer may leave the result in x0. We model memset with
; llvm.memset, which returns void, so x0 has to be installed by hand.

target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx14.0.0"

declare void @llvm.memset.p0.i64(ptr writeonly captures(none), i8, i64, i1 immarg)

define ptr @memset_returns_dest(ptr %p, i64 %n) {
  tail call void @llvm.memset.p0.i64(ptr align 4 %p, i8 0, i64 %n, i1 false)
  ret ptr %p
}
