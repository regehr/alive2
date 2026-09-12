; USHR on 64-bit vector lanes by 64 produces zero, with no LLVM poison.
; Expected constant was also checked on native AArch64 hardware.

; Expected X0: 0x0000000000000000.
define i64 @ushr_width_64() {
  ret i64 0
}
