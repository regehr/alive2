; Signed right shift by the lane width adds sign fill to the destination.
; Expected constant was also checked on native AArch64 hardware.

; Expected X0: 0x0000000000000004.
define i64 @ssra_width() {
  ret i64 4
}
