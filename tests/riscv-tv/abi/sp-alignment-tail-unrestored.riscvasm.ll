; CHECK: 1 incorrect transformations

; An unaligned tail call also violates the SP restoration contract.
declare i64 @callee()
define i64 @test() {
  %result = tail call i64 @callee()
  ret i64 %result
}
