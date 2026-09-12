; CHECK: 1 incorrect transformations

; A call gives the invalidated t0 a fresh unknown value.
declare void @clobber()
define i64 @test() {
  %anchor = add i64 0, 0
  call void @clobber()
  ret i64 0
}
