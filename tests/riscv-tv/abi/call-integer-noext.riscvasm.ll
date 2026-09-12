; CHECK: 1 correct transformations

; Unattributed call arguments do not require their excess bits to be extended.
declare void @consume(i8)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(i8 -1)
  ret void
}
