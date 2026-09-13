; CHECK: 1 correct transformations

; Restored LR is valid after an indirect call.
define i64 @test(ptr %callee) {
  %anchor = add i64 0, 0
  %result = call i64 %callee()
  ret i64 %result
}
