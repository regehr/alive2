; CHECK: 1 incorrect transformations

; The declaration alone can require sign extension of a call argument.
declare void @consume(i8 signext)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(i8 -1)
  ret void
}
