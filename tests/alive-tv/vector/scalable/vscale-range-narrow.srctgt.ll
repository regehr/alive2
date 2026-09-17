; TEST-ARGS: --max-vscale=4
; CHECK: Checking vscale = 4
; CHECK-NOT: Transformation seems to be correct!
; ERROR: Target vscale_range excludes vscale = 4

define i32 @src() vscale_range(1, 4) {
  ret i32 0
}
define i32 @tgt() vscale_range(1, 2) {
  ret i32 0
}
