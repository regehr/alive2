; CHECK: 1 incorrect transformations

; A signed i32 argument must be sign-extended to 64 bits.
declare void @consume(i32 signext)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(i32 signext -1)
  ret void
}
