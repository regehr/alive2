; Equal FCMP operands set NZCV to 0110: both Z and C are set.
; Expected constant was also checked on native AArch64 hardware.

; Expected X0: 0x0000000060000000.
define i64 @fcmp_equal_carry() {
  ret i64 1610612736
}
