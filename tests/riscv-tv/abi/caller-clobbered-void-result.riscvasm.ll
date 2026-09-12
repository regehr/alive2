; CHECK: 1 incorrect transformations

; Even a void callee may overwrite a0.
declare void @clobber()
define i64 @test() {
  %anchor = add i64 0, 0
  call void @clobber()
  ret i64 42
}
