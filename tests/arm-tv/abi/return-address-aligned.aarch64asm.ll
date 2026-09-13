; CHECK: 1 correct transformations

; An A64 caller's return address is already four-byte aligned.
define void @test() {
  ret void
}
