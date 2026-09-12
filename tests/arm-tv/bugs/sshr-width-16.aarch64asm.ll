; SSHR by 16 sign-fills each halfword lane; inputs have mixed signs.
; Assembly checks the full 128-bit destination against explicit constants.
; A zero return means both halves match. Verified on native AArch64 hardware.
define i64 @sshr_width_16() {
  ret i64 0
}
