; TEST-ARGS: --max-vscale=4 --exit-on-error
; CHECK: Checking vscale = 2
; CHECK-NOT: Checking vscale = 4
; CHECK-NOT: @src2
; CHECK-NOT: Transformation seems to be correct!
; ERROR: Value mismatch

define i32 @src1() {
  %v = call i32 @llvm.vscale.i32()
  ret i32 %v
}
define i32 @tgt1() {
  ret i32 1
}
define i32 @src2() {
  ret i32 0
}
define i32 @tgt2() {
  ret i32 0
}
