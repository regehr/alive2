; CHECK: 1 incorrect transformations

; A tail call must preserve its caller's return destination.
declare i64 @callee()
define i64 @test() {
  %anchor = add i64 0, 0
  %result = tail call i64 @callee()
  ret i64 %result
}
