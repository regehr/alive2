; CHECK: 1 correct transformations

; Tail calls are valid after restoring all modified saved registers.
declare i64 @callee()
define i64 @test() {
  %anchor = add i64 0, 0
  %result = tail call i64 @callee()
  ret i64 %result
}
