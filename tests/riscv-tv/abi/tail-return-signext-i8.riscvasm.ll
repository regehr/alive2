; CHECK: 1 correct transformations

; Keeping the source return type permits a narrow tail result to be checked.
declare signext i8 @callee()
define signext i8 @test() {
  %anchor = add i64 0, 0
  %value = tail call signext i8 @callee()
  ret i8 %value
}
