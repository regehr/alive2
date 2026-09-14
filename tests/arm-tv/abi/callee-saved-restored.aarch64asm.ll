; CHECK: 1 correct transformations

; A frame can modify saved state if it restores it before returning.
define void @test() {
  ret void
}
