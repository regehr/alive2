; TEST-ARGS: --max-vscale=2
; ERROR: Source is more defined than target
; CHECK: Checking vscale = 2
; CHECK-NOT: Transformation seems to be correct!

; At vscale=2, the i1 result is poison and noundef makes the call UB even
; though its result is unused. Ignoring noundef incorrectly validates it.
define i32 @src() {
  ret i32 0
}

define i32 @tgt() {
  %v = call noundef i1 @llvm.vscale.i1()
  ret i32 0
}
