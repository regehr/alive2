; CHECK: 1 incorrect transformations

; A tail call does not excuse an earlier temporary misalignment.
declare i64 @callee()
define i64 @test() {
  %result = tail call i64 @callee()
  ret i64 %result
}
