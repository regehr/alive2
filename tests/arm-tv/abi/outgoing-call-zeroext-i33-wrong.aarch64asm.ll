; CHECK: 1 incorrect transformations

; Outgoing zeroext i33 arguments constrain bits 63:33.
declare void @consume(i33)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(i33 zeroext 1)
  ret void
}
