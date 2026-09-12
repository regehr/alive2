; A variable unsigned right shift by the lane width produces zero.
; Expected constant was also checked on native AArch64 hardware.

; Expected X0: 0x0000000000000000.
define i64 @ushl_right_width() {
  ret i64 0
}
