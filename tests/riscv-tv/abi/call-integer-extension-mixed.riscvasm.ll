; CHECK: 1 correct transformations

; Valid mixed FP, signext and zeroext arguments retain their separate contracts.
declare void @consume(float, i8 signext, i32)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(float 1.0, i8 -1, i32 zeroext -1)
  ret void
}
