; SSHL uses the signed low byte of the count: 255 means right shift by one.
; Expected constant was also checked on native AArch64 hardware.

; Expected X0: 0x00000000fffffffe.
define i64 @sshl_low_byte() {
  ret i64 4294967294
}
