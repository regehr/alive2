; CHECK: 1 incorrect transformations

; A zero-extended byte argument cannot carry junk in the upper word.
declare void @consume(i8 zeroext)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(i8 zeroext -1)
  ret void
}
