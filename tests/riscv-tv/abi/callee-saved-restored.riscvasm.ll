; CHECK: 1 correct transformations

; Saved GPRs and FP registers survive calls and can be restored bit-for-bit.
declare void @clobber()
define i64 @test() {
  %anchor = add i64 0, 0
  call void @clobber()
  ret i64 42
}
