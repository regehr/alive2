; CHECK: 1 correct transformations

; A 16-byte call frame preserves the stack alignment contract.
declare i64 @callee()
define i64 @test() {
  %result = call i64 @callee()
  ret i64 %result
}
