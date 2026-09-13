; CHECK: 1 correct transformations

; Tail-call Boolean arguments require 0 or 1 in the low byte only.
declare void @consume(i1)
define void @test() {
  %anchor = add i64 0, 0
  tail call void @consume(i1 true)
  ret void
}
