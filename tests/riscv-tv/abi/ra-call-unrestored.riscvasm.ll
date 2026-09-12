; CHECK: 1 incorrect transformations
; CHECK-NOT: Transformation seems to be correct!

; A non-tail call overwrites ra, which must be restored before returning.
declare void @callee()
define void @test() {
  %anchor = add i64 0, 0
  call void @callee()
  ret void
}
