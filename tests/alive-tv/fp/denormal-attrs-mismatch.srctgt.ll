; ERROR: Function attributes not refined

; src has positivezero denormal mode; tgt has no attribute (defaults to ieee).
; These are genuinely different, so the transformation must be rejected.
define float @src(float %x) denormal_fpenv(positivezero) {
  ret float %x
}

define float @tgt(float %x) {
  ret float %x
}
