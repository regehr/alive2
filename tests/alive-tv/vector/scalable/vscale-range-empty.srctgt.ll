; TEST-ARGS: --max-vscale=4
; SKIP-IDENTITY
; ERROR: vscale_range minimum (8) exceeds the maximum vscale (4)
; CHECK-NOT: Transformation seems to be correct!
; CHECK-NOT: Checking vscale =

; Equal ranges can still exclude every requested scale, even without a
; scalable vector type. Reject this before entering the type checker.
define i32 @src(i32 %x) vscale_range(8,8) {
  ret i32 %x
}
define i32 @tgt(i32 %x) vscale_range(8,8) {
  ret i32 %x
}
