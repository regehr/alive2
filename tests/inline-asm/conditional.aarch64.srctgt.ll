; TEST-ARGS: -disable-undef-input -disable-poison-input
; SKIP-IDENTITY
target triple = "aarch64-unknown-linux-gnu"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"

declare i32 @llvm.abs.i32(i32, i1)

; ---- csel ----
define i32 @src(i32 %a, i32 %b, i32 %c) {
  %cmp = icmp sgt i32 %c, 0
  %r = select i1 %cmp, i32 %a, i32 %b
  ret i32 %r
}

define i32 @tgt(i32 %a, i32 %b, i32 %c) {
  %r = call i32 asm "cmp ${3:w}, #0\0Acsel ${0:w}, ${1:w}, ${2:w}, gt", "=r,r,r,r"(i32 %a, i32 %b, i32 %c)
  ret i32 %r
}

; ---- conditional_negate ----
define i32 @src_conditional_negate(i32 %val, i32 %cond) {
  %is_neg = icmp eq i32 %cond, 0
  %negated = sub i32 0, %val
  %r = select i1 %is_neg, i32 %negated, i32 %val
  ret i32 %r
}

define i32 @tgt_conditional_negate(i32 %val, i32 %cond) {
  %r = call i32 asm "cmp ${2:w}, #0\0Acsneg ${0:w}, ${1:w}, ${1:w}, ne", "=r,r,r,~{nzcv}"(i32 %val, i32 %cond)
  ret i32 %r
}

; ---- abs_idiom ----
define i32 @src_abs_idiom(i32 %a) {
  %r = call i32 @llvm.abs.i32(i32 %a, i1 false)
  ret i32 %r
}

define i32 @tgt_abs_idiom(i32 %a) {
  %r = call i32 asm "cmp ${1:w}, #0\0Acneg ${0:w}, ${1:w}, mi", "=r,r"(i32 %a)
  ret i32 %r
}

; ---- min_max_pair ----
define i32 @src_min_max_pair(i32 %x, i32 %lo, i32 %hi) {
  %cmp_lo = icmp slt i32 %x, %lo
  %s1 = select i1 %cmp_lo, i32 %lo, i32 %x
  %cmp_hi = icmp sgt i32 %s1, %hi
  %r = select i1 %cmp_hi, i32 %hi, i32 %s1
  ret i32 %r
}

define i32 @tgt_min_max_pair(i32 %x, i32 %lo, i32 %hi) {
  %r = call i32 asm "cmp ${1:w}, ${2:w}\0Acsel ${0:w}, ${2:w}, ${1:w}, lt\0Acmp ${0:w}, ${3:w}\0Acsel ${0:w}, ${3:w}, ${0:w}, gt", "=&r,r,r,r"(i32 %x, i32 %lo, i32 %hi)
  ret i32 %r
}
