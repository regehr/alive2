; CHECK: 1 incorrect transformations

; A tail call must meet its callee's extension requirements.
declare void @consume(i8 zeroext)
define void @test() {
  %anchor = add i64 0, 0
  tail call void @consume(i8 zeroext -1)
  ret void
}
