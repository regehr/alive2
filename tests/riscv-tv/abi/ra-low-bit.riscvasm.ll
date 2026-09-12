; CHECK: 1 correct transformations
; CHECK-NOT: ERROR:

; JALR ignores bit 0, including when ret uses the compressed encoding.
define void @test() {
  ret void
}
