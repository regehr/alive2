; TEST-ARGS: --max-vscale=4 --bidirectional
; SKIP-IDENTITY
; ERROR: Unsupported change to vscale_range
; CHECK: 0 correct transformations
; CHECK: 0 incorrect transformations
; CHECK: 6 Alive2 errors
; CHECK-NOT: Transformation seems to be correct!
; CHECK-NOT: Checking vscale =

; Reject both narrowing and widening, including changes outside the tested
; scales. The restriction also applies when there are no scalable vectors.
define i32 @src_lower(i32 %x) vscale_range(1,4) {
  ret i32 %x
}
define i32 @tgt_lower(i32 %x) vscale_range(2,4) {
  ret i32 %x
}

define i32 @src_upper(i32 %x) vscale_range(1,8) {
  ret i32 %x
}
define i32 @tgt_upper(i32 %x) vscale_range(1,4) {
  ret i32 %x
}

define i32 @src_widen(i32 %x) vscale_range(2,4) {
  ret i32 %x
}
define i32 @tgt_widen(i32 %x) vscale_range(1,4) {
  ret i32 %x
}

define i32 @src_add(i32 %x) {
  ret i32 %x
}
define i32 @tgt_add(i32 %x) vscale_range(1,0) {
  ret i32 %x
}

define i32 @src_remove(i32 %x) vscale_range(1,4) {
  ret i32 %x
}
define i32 @tgt_remove(i32 %x) {
  ret i32 %x
}

define i32 @src_unbounded(i32 %x) vscale_range(1,0) {
  ret i32 %x
}
define i32 @tgt_unbounded(i32 %x) vscale_range(1,4) {
  ret i32 %x
}
