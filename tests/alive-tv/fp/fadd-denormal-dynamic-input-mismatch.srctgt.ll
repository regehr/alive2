; ERROR: Value mismatch

; When the input mode is dynamic, the runtime environment may still require
; denormal inputs to be treated as zero, even though outputs are fixed at ieee.
define float @src() denormal_fpenv(ieee|dynamic) {
  %a = fdiv float 0x3810000000000000, 2.000000e+00
  %b = fadd float %a, 0.0
  ret float %b
}

define float @tgt() denormal_fpenv(ieee|dynamic) {
  %a = fdiv float 0x3810000000000000, 2.000000e+00
  ret float %a
}
