; TEST-ARGS: --max-vscale=4
; CHECK: Checking vscale = 1
; CHECK: Transformation seems to be correct!
; CHECK-NOT: Checking vscale = 2
; CHECK-NOT: Checking vscale = 4

; vscale_range limits the scales that are checked.
define i8 @src(<vscale x 2 x i8> %v) vscale_range(1,1) {
  %r = extractelement <vscale x 2 x i8> %v, i32 2
  ret i8 %r
}

define i8 @tgt(<vscale x 2 x i8> %v) vscale_range(1,1) {
  ret i8 poison
}
