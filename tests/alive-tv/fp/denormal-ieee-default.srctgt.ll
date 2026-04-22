; denormal_fpenv(ieee) should be equivalent to no attribute, since IEEE is alive2's
; default. This transformation should be VALID.
define float @src(float %x) denormal_fpenv(ieee) {
  ret float %x
}

define float @tgt(float %x) {
  ret float %x
}
