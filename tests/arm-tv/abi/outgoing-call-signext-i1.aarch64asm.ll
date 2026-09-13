; CHECK: 1 correct transformations

; The signext i1 call contract overrides the default Boolean representation.
declare void @consume(i1 signext)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(i1 true)
  ret void
}
