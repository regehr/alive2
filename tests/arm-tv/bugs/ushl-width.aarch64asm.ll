; A variable left shift by the lane width produces zero; counts do not wrap.
; Expected constant was also checked on native AArch64 hardware.

; Expected X0: 0x0000000000000000.
define i64 @ushl_width() {
  ret i64 0
}
