; CHECK: 1 incorrect transformations

; Both halves of v7 are caller-saved.
declare void @clobber()
define i64 @test() {
  %anchor = add i64 0, 0
  call void @clobber()
  ret i64 0
}
