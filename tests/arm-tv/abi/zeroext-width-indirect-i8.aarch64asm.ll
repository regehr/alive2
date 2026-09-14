; CHECK: 1 correct transformations

; Normalize an indirect call's narrow result before using the full register.
define i64 @test(ptr %callee) {
  %anchor = add i64 0, 0
  %value = call zeroext i8 %callee()
  %result = zext i8 %value to i64
  ret i64 %result
}
