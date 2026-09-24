; TEST-ARGS: --max-vscale=4 --disable-undef-input
; CHECK: Checking vscale = 1
; CHECK: Checking vscale = 2
; CHECK: Checking vscale = 4
; CHECK: Transformation seems to be correct!
; CHECK-NOT: Checking vscale = 8

; The type checker enumerates every power-of-two vscale up to the maximum.
define <vscale x 2 x i8> @src(<vscale x 2 x i8> %x, <vscale x 2 x i8> %y) {
  %a = and <vscale x 2 x i8> %x, %y
  %r = or <vscale x 2 x i8> %a, %x
  ret <vscale x 2 x i8> %r
}

define <vscale x 2 x i8> @tgt(<vscale x 2 x i8> %x, <vscale x 2 x i8> %y) {
  ret <vscale x 2 x i8> %x
}
