; CHECK: 1 correct transformations

; LR may be overwritten if RET uses the saved return address.
define void @test() {
  ret void
}
