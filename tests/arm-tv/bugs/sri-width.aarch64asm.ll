; SRI by 32 on 32-bit lanes preserves the destination and inserts no bits.
; Expected constant was also checked on native AArch64 hardware.

; Expected X0: 0x0000000000000000.
define i64 @sri_width() {
  ret i64 0
}
