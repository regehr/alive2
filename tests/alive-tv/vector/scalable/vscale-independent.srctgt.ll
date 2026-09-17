; CHECK: Transformation seems to be correct!
; CHECK-NOT: Checking vscale
; CHECK-NOT: ERROR:

; Neither an unused declaration nor unrelated functions trigger enumeration.
declare i32 @llvm.vscale.i32()
define i32 @unrelated() {
  %v = call i32 @llvm.vscale.i32()
  ret i32 %v
}
define i32 @src(i32 %vscale) {
  ret i32 %vscale
}
define i32 @tgt(i32 %vscale) {
  %v = add i32 %vscale, 0
  ret i32 %v
}
