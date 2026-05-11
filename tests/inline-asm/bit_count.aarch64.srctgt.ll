; TEST-ARGS: -disable-undef-input -disable-poison-input
; SKIP-IDENTITY
target triple = "aarch64-unknown-linux-gnu"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"

declare i32 @llvm.ctlz.i32(i32, i1)
declare i64 @llvm.ctlz.i64(i64, i1)
declare i32 @llvm.cttz.i32(i32, i1)
declare i32 @llvm.bitreverse.i32(i32)
declare i32 @llvm.bswap.i32(i32)
declare i8 @llvm.ctpop.i8(i8)

; ---- clz ----
define i32 @src(i32 %a) {
  %r = call i32 @llvm.ctlz.i32(i32 %a, i1 false)
  ret i32 %r
}

define i32 @tgt(i32 %a) {
  %r = call i32 asm "clz ${0:w}, ${1:w}", "=r,r"(i32 %a)
  ret i32 %r
}

; ---- ctlz64 ----
define i64 @src_ctlz64(i64 %a) {
  %r = call i64 @llvm.ctlz.i64(i64 %a, i1 false)
  ret i64 %r
}

define i64 @tgt_ctlz64(i64 %a) {
  %r = call i64 asm "clz $0, $1", "=r,r"(i64 %a)
  ret i64 %r
}

; ---- ctz_via_rbit_clz ----
define i32 @src_ctz_via_rbit_clz(i32 %a) {
  %r = call i32 @llvm.cttz.i32(i32 %a, i1 false)
  ret i32 %r
}

define i32 @tgt_ctz_via_rbit_clz(i32 %a) {
  %r = call i32 asm "rbit ${0:w}, ${1:w}\0Aclz ${0:w}, ${0:w}", "=&r,r"(i32 %a)
  ret i32 %r
}

; ---- rbit ----
define i32 @src_rbit(i32 %a) {
  %r = call i32 @llvm.bitreverse.i32(i32 %a)
  ret i32 %r
}

define i32 @tgt_rbit(i32 %a) {
  %r = call i32 asm "rbit ${0:w}, ${1:w}", "=r,r"(i32 %a)
  ret i32 %r
}

; ---- rev ----
define i32 @src_rev(i32 %a) {
  %r = call i32 @llvm.bswap.i32(i32 %a)
  ret i32 %r
}

define i32 @tgt_rev(i32 %a) {
  %r = call i32 asm "rev ${0:w}, ${1:w}", "=r,r"(i32 %a)
  ret i32 %r
}

; ---- popcount ----
define i8 @src_popcount(i8 %a) {
  %r = call i8 @llvm.ctpop.i8(i8 %a)
  ret i8 %r
}

define i8 @tgt_popcount(i8 %a) {
  %ext = zext i8 %a to i32
  %cnt = call i32 asm "fmov s0, ${1:w}\0Acnt v0.8b, v0.8b\0Afmov ${0:w}, s0", "=r,r,~{v0}"(i32 %ext)
  %trunc = trunc i32 %cnt to i8
  ret i8 %trunc
}
