; TEST-ARGS: --max-vscale=4 --always-verify=false --bidirectional
; ERROR: Unsupported change to vscale_range
; CHECK-NOT: Transformation seems to be correct!
; CHECK-NOT: Checking vscale =

; The printed Alive IR is identical: reject the changed range before the
; syntactic-equality shortcut.
define <vscale x 2 x i8> @f(<vscale x 2 x i8> %x) vscale_range(1,4) {
  ret <vscale x 2 x i8> %x
}
