; TEST-ARGS: --max-vscale=4
; SKIP-IDENTITY
; ERROR: vscale_range minimum (8) exceeds the maximum vscale (4)
; CHECK-NOT: Transformation seems to be correct!
; CHECK-NOT: Checking vscale =

; Equal ranges can still exclude every requested scale. Reject this before
; entering the type checker.
define <vscale x 2 x i8> @src(<vscale x 2 x i8> %x) vscale_range(8,8) {
  ret <vscale x 2 x i8> %x
}
define <vscale x 2 x i8> @tgt(<vscale x 2 x i8> %x) vscale_range(8,8) {
  ret <vscale x 2 x i8> %x
}
