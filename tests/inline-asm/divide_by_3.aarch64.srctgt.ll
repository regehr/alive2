; TEST-ARGS: -disable-undef-input -disable-poison-input
; SKIP-IDENTITY
target triple = "aarch64-unknown-linux-gnu"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"

; Unsigned divide by 3 using multiply-high trick:
; x / 3 = (x * 0xAAAAAAAB) >> 33  (for 32-bit unsigned)
; LLVM knows this trick, but here we verify the asm version is correct.
; src: straightforward udiv
; tgt: the classic multiply-and-shift asm idiom

define i32 @src(i32 %x) {
  %r = udiv i32 %x, 3
  ret i32 %r
}

define i32 @tgt(i32 %x) {
  %r = call i32 asm "mov ${0:w}, #0xaaab\0Amovk ${0:w}, #0xaaaa, lsl #16\0Aumull $0, ${0:w}, ${1:w}\0Alsr $0, $0, #33\0Amov ${0:w}, ${0:w}", "=&r,r"(i32 %x)
  ret i32 %r
}
