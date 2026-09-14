; CHECK: 1 incorrect transformations

; x1 is caller-saved in the supported Linux ABI, including void calls.
declare void @clobber()
define i64 @test() {
  %anchor = add i64 0, 0
  call void @clobber()
  ret i64 42
}
