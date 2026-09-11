; XFAIL: ERROR: Value mismatch
; LLVM lowers (a >= c) | (b >= c) to FMAXNM followed by a comparison.
; With a=c=+0 and b=sNaN, the source returns true but FMAXNM produces a
; quiet NaN, so the target returns false (confirmed on AArch64 hardware).
; ModuleID = '<stdin>'
source_filename = "<stdin>"

; Function Attrs: nounwind
define i1 @f(float %0, float %1, float %2) {
  %4 = fcmp oge float %0, %2
  %5 = fcmp oge float %1, %2
  %6 = or i1 %4, %5
  ret i1 %6
}

