; TEST-ARGS: --max-vscale=2
; SKIP-IDENTITY
; ERROR: No vscale values to check up to 2
; CHECK-NOT: Transformation seems to be correct!

define i32 @src() vscale_range(4) {
  ret i32 0
}
define i32 @tgt() vscale_range(4) {
  ret i32 0
}
