; ERROR: Value mismatch

; The f32-specific denormal mode is ieee, so subnormal float32 results are not flushed.
; The default mode for other types is positivezero, but that does not apply to float32.
; Therefore the fdiv result (a subnormal) cannot be folded to 0.0.
define float @src() denormal_fpenv(positivezero, float: ieee) {
  %result = fdiv float 0x3810000000000000, 2.000000e+00
  ret float %result
}

define float @tgt() denormal_fpenv(positivezero, float: ieee) {
  ret float 0.0
}
