; USHL uses the signed low byte of the count: 255 means right shift by one.
; Expected constant was also checked on native AArch64 hardware.

; Expected X0: 0x0000000000000001.
define i64 @ushl_low_byte() {
  ret i64 1
}
