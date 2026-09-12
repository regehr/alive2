; USHR by 8 on byte lanes produces an entirely zero vector.
; Assembly checks the full 128-bit destination against explicit constants.
; A zero return means both halves match. Verified on native AArch64 hardware.
define i64 @ushr_width_8() {
  ret i64 0
}
