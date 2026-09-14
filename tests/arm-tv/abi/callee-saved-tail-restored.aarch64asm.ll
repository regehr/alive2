; CHECK: 1 correct transformations

; Tail branches preserve LR after the caller's frame is restored.
declare i64 @callee()
define i64 @test() {
  %anchor = add i64 0, 0
  %result = tail call i64 @callee()
  ret i64 %result
}
