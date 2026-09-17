; TEST-ARGS: --max-vscale=4 --bidirectional
; CHECK-NOT: Transformation seems to be correct!
; ERROR: Source vscale_range excludes vscale = 1

define i32 @src() vscale_range(2) {
  ret i32 0
}
define i32 @tgt() vscale_range(1, 2) {
  ret i32 0
}
