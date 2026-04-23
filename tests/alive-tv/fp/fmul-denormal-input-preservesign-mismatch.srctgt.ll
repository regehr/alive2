; ERROR: Value mismatch

; preserve-sign input handling is mandatory and must preserve the sign when a
; denormal input is treated as zero.
define float @src() denormal_fpenv(ieee|preservesign) {
  %a = fdiv float 0x3810000000000000, -2.000000e+00
  %b = fmul float %a, 1.000000e+00
  ret float %b
}

define float @tgt() denormal_fpenv(ieee|preservesign) {
  %a = fdiv float 0x3810000000000000, -2.000000e+00
  ret float %a
}
