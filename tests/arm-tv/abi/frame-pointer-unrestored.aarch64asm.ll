; CHECK: 1 incorrect transformations

; x29 is callee-saved even when no frame chain is maintained.
define void @test() {
  ret void
}
