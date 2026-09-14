; CHECK: 1 incorrect transformations

; An indirect tail call must preserve the caller's return destination.
define i64 @test(ptr %callee) {
  %anchor = add i64 0, 0
  %result = tail call i64 %callee()
  ret i64 %result
}
