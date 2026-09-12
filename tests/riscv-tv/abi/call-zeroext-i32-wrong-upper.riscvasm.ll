; CHECK: 1 incorrect transformations

; An explicit zeroext word argument requires a zero upper word.
declare void @consume(i32 zeroext)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(i32 zeroext -1)
  ret void
}
