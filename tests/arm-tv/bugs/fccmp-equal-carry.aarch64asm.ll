; A taken FCCMP comparison of equal operands sets NZCV to 0110.
; Expected constant was also checked on native AArch64 hardware.

; Expected X0: 0x0000000060000000.
define i64 @fccmp_equal_carry() {
  ret i64 1610612736
}
