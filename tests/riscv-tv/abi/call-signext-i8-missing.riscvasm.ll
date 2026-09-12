; CHECK: 1 incorrect transformations

; Truncating a0 to i8 must not hide missing sign extension.
declare void @consume(i8 signext)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(i8 signext -1)
  ret void
}
