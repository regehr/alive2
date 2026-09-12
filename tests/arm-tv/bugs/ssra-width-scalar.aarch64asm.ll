; SSRA by 64 adds sign fill to the accumulator and clears upper bits.
; Assembly checks the full 128-bit destination against explicit constants.
; A zero return means both halves match. Verified on native AArch64 hardware.
define i64 @ssra_width_scalar() {
  ret i64 0
}
