; A variable signed right shift by the lane width produces sign fill.
; Expected constant was also checked on native AArch64 hardware.

; Expected X0: 0x00000000ffffffff.
define i64 @sshl_right_width() {
  ret i64 4294967295
}
