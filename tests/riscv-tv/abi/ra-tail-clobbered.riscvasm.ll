; CHECK: 1 incorrect transformations
; CHECK-NOT: Transformation seems to be correct!

; A tail callee must receive the original caller's return destination.
declare void @callee()
define void @test() {
  %anchor = add i64 0, 0
  tail call void @callee()
  ret void
}
