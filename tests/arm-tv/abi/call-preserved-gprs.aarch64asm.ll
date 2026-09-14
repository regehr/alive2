; CHECK: 1 correct transformations

; Callee-saved GPRs retain all 64 bits across an external call.
declare void @clobber()
define i64 @test(i64 noundef %a, i64 noundef %b) {
  %anchor = add i64 0, 0
  call void @clobber()
  %result = xor i64 %a, %b
  ret i64 %result
}
