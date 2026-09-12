; CHECK: 1 correct transformations
; CHECK-NOT: ERROR:

; Check the address passed to the tail callee, ignoring JALR's low bit.
declare void @callee()
define void @test() {
  %anchor = add i64 0, 0
  tail call void @callee()
  ret void
}
