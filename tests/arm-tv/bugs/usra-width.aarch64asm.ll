; Unsigned right shift by the lane width adds zero to the destination.
; Expected constant was also checked on native AArch64 hardware.

; Expected X0: 0x0000000000000005.
define i64 @usra_width() {
  ret i64 5
}
