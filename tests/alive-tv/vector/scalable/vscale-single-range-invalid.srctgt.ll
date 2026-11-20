; TEST-ARGS: --single-vscale=4 --always-verify=false --bidirectional
; SKIP-IDENTITY
; ERROR: single-vscale (4) is outside vscale_range(8,0)
; CHECK: ERROR: single-vscale (4) is outside vscale_range(1,2)
; CHECK: 0 correct transformations
; CHECK: 0 incorrect transformations
; CHECK: 2 Alive2 errors
; CHECK-NOT: Transformation seems to be correct!
; CHECK-NOT: These functions seem to be equivalent!

; Check both bounds before the syntactic-equality shortcut. An unbounded
; maximum still has a lower bound.
define <vscale x 2 x i8> @src_lower(<vscale x 2 x i8> %x) vscale_range(8,0) {
  ret <vscale x 2 x i8> %x
}
define <vscale x 2 x i8> @tgt_lower(<vscale x 2 x i8> %x) vscale_range(8,0) {
  ret <vscale x 2 x i8> %x
}

define <vscale x 2 x i8> @src_upper(<vscale x 2 x i8> %x) vscale_range(1,2) {
  ret <vscale x 2 x i8> %x
}
define <vscale x 2 x i8> @tgt_upper(<vscale x 2 x i8> %x) vscale_range(1,2) {
  ret <vscale x 2 x i8> %x
}
