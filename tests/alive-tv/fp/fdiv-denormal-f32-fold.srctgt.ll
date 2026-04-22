; The f32-specific denormal mode is positivezero (default/non-f32 mode is ieee).
; A float32 fdiv whose result is a positive subnormal folds to +0.0.
define float @src() denormal_fpenv(float: positivezero) {
  %result = fdiv float 0x3810000000000000, 2.000000e+00
  ret float %result
}

define float @tgt() denormal_fpenv(float: positivezero) {
  ret float 0.0
}
