; CHECK: 1 correct transformations

; A wide zeroext argument with clear excess bits is valid.
declare void @consume(i33)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(i33 zeroext 1)
  ret void
}
