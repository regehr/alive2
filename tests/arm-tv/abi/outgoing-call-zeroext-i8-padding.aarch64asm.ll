; CHECK: 1 correct transformations

; Outgoing zeroext i8 arguments leave bits 63:32 unspecified.
declare void @consume(i8)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(i8 zeroext 1)
  ret void
}
