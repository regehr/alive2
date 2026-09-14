; CHECK: 1 incorrect transformations

; The call site alone can require zero extension of a call argument.
declare void @consume(i8)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(i8 zeroext 1)
  ret void
}
