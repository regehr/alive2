; CHECK: 1 correct transformations

; Sign extension of a wide outgoing integer includes the upper word.
declare void @consume(i33 signext)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(i33 -1)
  ret void
}
