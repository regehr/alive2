; ERROR: Value mismatch

; Dynamic output and input denormal modes are independent. The output mode may
; leave the fdiv result subnormal while the input mode still forces the fadd to
; consume it as zero.
define float @src() denormal_fpenv(dynamic) {
  %a = fdiv float 0x3810000000000000, 2.000000e+00
  %b = fadd float %a, 0.0
  ret float %b
}

define float @tgt() denormal_fpenv(dynamic) {
  %a = fdiv float 0x3810000000000000, 2.000000e+00
  ret float %a
}
