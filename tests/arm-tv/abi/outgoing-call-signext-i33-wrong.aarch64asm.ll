; CHECK: 1 incorrect transformations

; Outgoing signext i33 arguments must be extended to 64 bits.
declare void @consume(i33 signext)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(i33 -1)
  ret void
}
