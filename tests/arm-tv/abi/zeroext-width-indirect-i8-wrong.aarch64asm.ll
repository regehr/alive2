; CHECK: 1 incorrect transformations

; The upper word is unspecified after indirect calls too.
define i64 @test(ptr %callee) {
  %anchor = add i64 0, 0
  %value = call zeroext i8 %callee()
  %result = zext i8 %value to i64
  ret i64 %result
}
