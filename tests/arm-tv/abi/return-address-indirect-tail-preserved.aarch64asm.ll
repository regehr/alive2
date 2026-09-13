; CHECK: 1 correct transformations

; BR tail calls do not overwrite LR.
define i64 @test(ptr %callee) {
  %anchor = add i64 0, 0
  %result = tail call i64 %callee()
  ret i64 %result
}
