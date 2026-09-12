; SRI by 64 on 64-bit lanes preserves the destination and inserts no bits.
; Expected constant was also checked on native AArch64 hardware.

; Expected X0: 0x0000000000000002.
define i64 @sri_width_64() {
  ret i64 2
}
