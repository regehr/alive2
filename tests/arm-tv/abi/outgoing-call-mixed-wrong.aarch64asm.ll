; CHECK: 1 incorrect transformations

; Use the parameter index for attributes when FP and integer arguments mix.
declare void @consume(float, i8 signext, i8)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(float 1.0, i8 -1, i8 zeroext 1)
  ret void
}
