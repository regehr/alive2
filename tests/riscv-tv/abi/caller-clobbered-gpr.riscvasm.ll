; CHECK: 1 incorrect transformations

; Argument registers do not preserve live values across calls.
declare void @clobber()
define i64 @test() {
  %anchor = add i64 0, 0
  call void @clobber()
  ret i64 42
}
