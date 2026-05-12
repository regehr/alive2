; TEST-ARGS: -disable-undef-input -disable-poison-input
; SKIP-IDENTITY
target triple = "aarch64-unknown-linux-gnu"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"

; ---- is_power_of_2 ----
define i1 @src(i32 %x) {
  %nz = icmp ne i32 %x, 0
  %dec = sub i32 %x, 1
  %and = and i32 %x, %dec
  %no_other_bits = icmp eq i32 %and, 0
  %r = and i1 %nz, %no_other_bits
  ret i1 %r
}

define i1 @tgt(i32 %x) {
  %wide = call i32 asm "sub ${0:w}, ${1:w}, #1\0Atst ${1:w}, ${0:w}\0Accmp ${1:w}, #0, #4, eq\0Acset ${0:w}, ne", "=&r,r,~{nzcv}"(i32 %x)
  %r = trunc i32 %wide to i1
  ret i1 %r
}

; ---- sat_add_u8 ----
define i32 @src_sat_add_u8(i32 %a, i32 %b) {
  %a8 = and i32 %a, 255
  %b8 = and i32 %b, 255
  %sum = add i32 %a8, %b8
  %overflow = icmp ugt i32 %sum, 255
  %r = select i1 %overflow, i32 255, i32 %sum
  ret i32 %r
}

define i32 @tgt_sat_add_u8(i32 %a, i32 %b) {
  %a8 = and i32 %a, 255
  %b8 = and i32 %b, 255
  %sum = add i32 %a8, %b8
  %r = call i32 asm "cmp ${1:w}, #255\0Amov ${0:w}, #255\0Acsel ${0:w}, ${0:w}, ${1:w}, hi", "=&r,r,~{nzcv}"(i32 %sum)
  ret i32 %r
}

; ---- blend_bytes ----
define i32 @src_blend_bytes(i32 %a, i32 %b, i32 %mask) {
  %sel_a = and i32 %a, %mask
  %not_mask = xor i32 %mask, -1
  %sel_b = and i32 %b, %not_mask
  %r = or i32 %sel_a, %sel_b
  ret i32 %r
}

define i32 @tgt_blend_bytes(i32 %a, i32 %b, i32 %mask) {
  %r = call i32 asm "and ${0:w}, ${1:w}, ${3:w}\0Abic ${1:w}, ${2:w}, ${3:w}\0Aorr ${0:w}, ${0:w}, ${1:w}", "=&r,r,r,r"(i32 %a, i32 %b, i32 %mask)
  ret i32 %r
}

; ---- midpoint_no_overflow ----
define i32 @src_midpoint_no_overflow(i32 %a, i32 %b) {
  %and = and i32 %a, %b
  %xor = xor i32 %a, %b
  %half_xor = lshr i32 %xor, 1
  %r = add i32 %and, %half_xor
  ret i32 %r
}

define i32 @tgt_midpoint_no_overflow(i32 %a, i32 %b) {
  %r = call i32 asm "and ${0:w}, ${1:w}, ${2:w}\0Aeor ${1:w}, ${1:w}, ${2:w}\0Aadd ${0:w}, ${0:w}, ${1:w}, lsr #1", "=&r,r,r"(i32 %a, i32 %b)
  ret i32 %r
}

; ---- complex_expr ----
define i32 @src_complex_expr(i32 %a, i32 %b, i32 %c, i32 %d) {
  %mul = mul i32 %a, %b
  %add = add i32 %mul, %c
  %r = xor i32 %add, %d
  ret i32 %r
}

define i32 @tgt_complex_expr(i32 %a, i32 %b, i32 %c, i32 %d) {
  %r = call i32 asm "madd ${0:w}, ${1:w}, ${2:w}, ${3:w}\0Aeor ${0:w}, ${0:w}, ${4:w}", "=&r,r,r,r,r"(i32 %a, i32 %b, i32 %c, i32 %d)
  ret i32 %r
}
