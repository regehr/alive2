; TEST-ARGS: --single-vscale=4 --always-verify=false --bidirectional
; SKIP-IDENTITY
; ERROR: single-vscale (4) is outside vscale_range(8,0)
; CHECK: ERROR: single-vscale (4) is outside vscale_range(1,2)
; CHECK: 0 correct transformations
; CHECK: 0 incorrect transformations
; CHECK: 2 Alive2 errors
; CHECK-NOT: Transformation seems to be correct!
; CHECK-NOT: These functions seem to be equivalent!

; Check both bounds before the syntactic-equality shortcut, even without
; scalable vector operations. An unbounded maximum still has a lower bound.
define i32 @src_lower(i32 %x) vscale_range(8,0) {
  ret i32 %x
}
define i32 @tgt_lower(i32 %x) vscale_range(8,0) {
  ret i32 %x
}

define i32 @src_upper(i32 %x) vscale_range(1,2) {
  ret i32 %x
}
define i32 @tgt_upper(i32 %x) vscale_range(1,2) {
  ret i32 %x
}
