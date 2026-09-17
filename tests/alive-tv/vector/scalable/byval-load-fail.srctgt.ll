; TEST-ARGS: --max-vscale=2
; ERROR: Value mismatch
; CHECK: Checking vscale = 2
; CHECK-NOT: Source function is always UB
; CHECK-NOT: Transformation seems to be correct!

; At vscale=2, byval provides 32 bytes, so the load is valid and lane 4
; can be nonzero. Using the 16-byte minimum incorrectly makes the load UB
; and allows replacing the extracted value with any constant.
define i32 @src(ptr byval(<vscale x 4 x i32>) %p) vscale_range(2) {
  %v = load <vscale x 4 x i32>, ptr %p, align 1
  %e = extractelement <vscale x 4 x i32> %v, i32 4
  ret i32 %e
}

define i32 @tgt(ptr byval(<vscale x 4 x i32>) %p) vscale_range(2) {
  ret i32 0
}
