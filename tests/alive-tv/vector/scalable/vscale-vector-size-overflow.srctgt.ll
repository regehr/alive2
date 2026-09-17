; TEST-ARGS: --max-vscale=2147483648
; SKIP-IDENTITY
; CHECK: Checking vscale = 2147483648
; CHECK-NOT: Transformation seems to be correct!
; ERROR: Vector type is too large

define <vscale x 2 x i8> @src(<vscale x 2 x i8> %v) vscale_range(2147483648) {
  ret <vscale x 2 x i8> %v
}
define <vscale x 2 x i8> @tgt(<vscale x 2 x i8> %v) vscale_range(2147483648) {
  ret <vscale x 2 x i8> %v
}
