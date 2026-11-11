; TEST-ARGS: --single-vscale=4 --bidirectional
; SKIP-IDENTITY
; CHECK: 4 correct transformations
; CHECK: 0 incorrect transformations
; CHECK: 0 failed-to-prove transformations
; CHECK: 0 Alive2 errors
; CHECK-NOT: ERROR:
; CHECK-NOT: Failed to verify the reverse transformation
; CHECK-NOT: Reverse transformation doesn't verify!

; Both bounds are inclusive, a single argument denotes an exact scale,
; and a zero maximum is unbounded.
define i32 @src_exact() vscale_range(4) {
  %v = call i32 @llvm.vscale.i32()
  ret i32 %v
}
define i32 @tgt_exact() vscale_range(4) {
  ret i32 4
}

define i32 @src_lower() vscale_range(4,8) {
  %v = call i32 @llvm.vscale.i32()
  ret i32 %v
}
define i32 @tgt_lower() vscale_range(4,8) {
  ret i32 4
}

define i32 @src_upper() vscale_range(2,4) {
  %v = call i32 @llvm.vscale.i32()
  ret i32 %v
}
define i32 @tgt_upper() vscale_range(2,4) {
  ret i32 4
}

define i32 @src_unbounded() vscale_range(4,0) {
  %v = call i32 @llvm.vscale.i32()
  ret i32 %v
}
define i32 @tgt_unbounded() vscale_range(4,0) {
  ret i32 4
}
