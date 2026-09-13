; CHECK: 1 correct transformations

; Saving LR and FP across a normal call preserves the caller's state.
declare i64 @callee()
define i64 @test() {
  %anchor = add i64 0, 0
  %result = call i64 @callee()
  ret i64 %result
}
