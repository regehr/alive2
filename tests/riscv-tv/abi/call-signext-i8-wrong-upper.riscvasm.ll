; CHECK: 1 incorrect transformations

; Sign extension must cover the upper word at a call, too.
declare void @consume(i8 signext)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(i8 signext -1)
  ret void
}
