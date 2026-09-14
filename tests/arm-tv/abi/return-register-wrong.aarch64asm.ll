; CHECK: 1 incorrect transformations

; RET must check its actual operand, even if LR remains intact.
define void @test() {
  ret void
}
