; ERROR: Value mismatch

; Input denormal handling is mandatory. The fadd must see +0.0, not the
; subnormal value produced by the preceding fdiv.
define float @src() denormal_fpenv(ieee|positivezero) {
  %a = fdiv float 0x3810000000000000, 2.000000e+00
  %b = fadd float %a, 0.0
  ret float %b
}

define float @tgt() denormal_fpenv(ieee|positivezero) {
  %a = fdiv float 0x3810000000000000, 2.000000e+00
  ret float %a
}
