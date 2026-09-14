; CHECK: 1 incorrect transformations

; x28 is the last ordinary callee-saved GPR.
define void @test() {
  ret void
}
