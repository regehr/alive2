; CHECK: 1 incorrect transformations

; A tail call must restore the incoming SP.
declare i64 @callee()
define i64 @test() {
  %anchor = add i64 0, 0
  %result = tail call i64 @callee()
  ret i64 %result
}
