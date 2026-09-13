; CHECK: 1 correct transformations

; An aligned temporary frame is restored before a tail call.
declare i64 @callee()
define i64 @test() {
  %result = tail call i64 @callee()
  ret i64 %result
}
