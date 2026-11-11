; SKIP-IDENTITY
; ERROR: single-vscale (2) is outside vscale_range(1,1)
; CHECK-NOT: Transformation seems to be correct!
; CHECK-NOT: Target is more poisonous than source

; This transformation is correct at the only permitted scale, 1. The default
; scale, 2, must be rejected instead of producing a spurious counterexample.
define i8 @src(<vscale x 2 x i8> %v) vscale_range(1) {
  %r = extractelement <vscale x 2 x i8> %v, i32 2
  ret i8 %r
}

define i8 @tgt(<vscale x 2 x i8> %v) vscale_range(1) {
  ret i8 poison
}
