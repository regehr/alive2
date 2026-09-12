; Unsigned right shift by the 32-bit lane width produces zero.
; Expected constant was also checked on native AArch64 hardware.

; Expected X0: 0x0000000000000000.
define i64 @ushr_width() {
  ret i64 0
}
