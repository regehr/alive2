; CHECK: 1 incorrect transformations

; Tail calls must restore the caller's FP.
declare i64 @callee()
define i64 @test() {
  %anchor = add i64 0, 0
  %result = tail call i64 @callee()
  ret i64 %result
}
