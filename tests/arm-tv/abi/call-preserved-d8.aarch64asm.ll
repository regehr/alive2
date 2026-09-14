; CHECK: 1 correct transformations

; The low half of v8 preserves raw bits, including NaN encodings.
declare void @clobber()
define i64 @test(i64 noundef %bits) {
  %anchor = add i64 0, 0
  call void @clobber()
  ret i64 %bits
}
