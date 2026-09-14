; CHECK: 1 incorrect transformations

; BL overwrites LR; the original return address must be recovered.
declare i64 @callee()
define i64 @test() {
  %anchor = add i64 0, 0
  %result = call i64 @callee()
  ret i64 %result
}
