; CHECK: 1 incorrect transformations

; Tail exits must preserve the caller's saved FP registers.
declare void @clobber()
define void @test() {
  %anchor = add i64 0, 0
  call void @clobber()
  ret void
}
