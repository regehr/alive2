; CHECK: 1 incorrect transformations

; x9 is invalidated by the external call.
declare void @clobber()
define i64 @test() {
  %anchor = add i64 0, 0
  call void @clobber()
  ret i64 0
}
