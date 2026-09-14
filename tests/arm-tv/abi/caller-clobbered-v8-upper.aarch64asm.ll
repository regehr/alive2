; CHECK: 1 incorrect transformations

; Preservation of d8 does not preserve the upper half of v8.
declare void @clobber()
define i64 @test() {
  %anchor = add i64 0, 0
  call void @clobber()
  ret i64 0
}
