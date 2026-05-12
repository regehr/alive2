; TEST-ARGS: -disable-undef-input -disable-poison-input
; SKIP-IDENTITY
target triple = "aarch64-unknown-linux-gnu"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"

; ---- bfi ----
define i32 @src(i32 %a, i32 %b) {
  %mask_a = and i32 %a, -65281       ; clear bits [8:15]: ~0xFF00
  %field = and i32 %b, 255           ; isolate low 8 bits of b
  %shifted = shl i32 %field, 8       ; shift into position
  %r = or i32 %mask_a, %shifted
  ret i32 %r
}

define i32 @tgt(i32 %a, i32 %b) {
  %r = call i32 asm "bfi ${0:w}, ${2:w}, #8, #8", "=r,0,r"(i32 %a, i32 %b)
  ret i32 %r
}

; ---- ubfx ----
define i32 @src_ubfx(i32 %a) {
  %shift = lshr i32 %a, 8
  %r = and i32 %shift, 255
  ret i32 %r
}

define i32 @tgt_ubfx(i32 %a) {
  %r = call i32 asm "ubfx ${0:w}, ${1:w}, #8, #8", "=r,r"(i32 %a)
  ret i32 %r
}

; ---- sxt_pattern ----
define i32 @src_sxt_pattern(i32 %a) {
  %trunc = trunc i32 %a to i8
  %ext = sext i8 %trunc to i32
  ret i32 %ext
}

define i32 @tgt_sxt_pattern(i32 %a) {
  %r = call i32 asm "sxtb ${0:w}, ${1:w}", "=r,r"(i32 %a)
  ret i32 %r
}

; ---- uxt_pattern ----
define i32 @src_uxt_pattern(i32 %a) {
  %r = and i32 %a, 65535
  ret i32 %r
}

define i32 @tgt_uxt_pattern(i32 %a) {
  %r = call i32 asm "uxth ${0:w}, ${1:w}", "=r,r"(i32 %a)
  ret i32 %r
}

; ---- extract_sign ----
define i32 @src_extract_sign(i32 %x) {
  %neg = icmp slt i32 %x, 0
  %r = select i1 %neg, i32 -1, i32 1
  ret i32 %r
}

define i32 @tgt_extract_sign(i32 %x) {
  %r = call i32 asm "cmp ${1:w}, #0\0Amov ${0:w}, #1\0Acsneg ${0:w}, ${0:w}, ${0:w}, ge", "=&r,r,~{nzcv}"(i32 %x)
  ret i32 %r
}

; ---- deposit_bits ----
define i32 @src_deposit_bits(i32 %dst, i32 %val) {
  %cleared = and i32 %dst, -61441     ; ~(0xF << 12) = 0xFFFF0FFF
  %field = and i32 %val, 15           ; isolate bits [0:3]
  %placed = shl i32 %field, 12        ; move to position [12:15]
  %r = or i32 %cleared, %placed
  ret i32 %r
}

define i32 @tgt_deposit_bits(i32 %dst, i32 %val) {
  %r = call i32 asm "bfi ${0:w}, ${2:w}, #12, #4", "=r,0,r"(i32 %dst, i32 %val)
  ret i32 %r
}
