; CHECK: 1 correct transformations

; No extension is required for an unattributed outgoing i8.
declare void @consume(i8)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(i8 1)
  ret void
}
