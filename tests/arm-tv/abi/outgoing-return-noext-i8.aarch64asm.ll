; CHECK: 1 correct transformations

; Unattributed i8 returns leave all bits above bit 7 unspecified.
define i8 @test() {
  ret i8 1
}
