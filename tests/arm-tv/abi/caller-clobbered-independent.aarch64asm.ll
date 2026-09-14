; CHECK: 1 incorrect transformations

; Separate calls produce independent unknown values in caller-saved registers.
declare void @clobber()
define i64 @test() {
  %anchor = add i64 0, 0
  call void @clobber()
  call void @clobber()
  ret i64 0
}
