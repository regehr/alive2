; TEST-ARGS: --max-vscale=1
; ERROR: Target is more poisonous than source
; CHECK: Checking vscale = 1
; CHECK-NOT: Transformation seems to be correct!

; At vscale=1, the target's range attribute turns the result into poison.
; Ignoring the attribute incorrectly validates replacing the constant with it.
define i32 @src() {
  ret i32 1
}

define i32 @tgt() {
  %v = call range(i32 2, 3) i32 @llvm.vscale.i32()
  ret i32 %v
}
