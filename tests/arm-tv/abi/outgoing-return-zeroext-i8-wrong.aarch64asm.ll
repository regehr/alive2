; CHECK: 1 incorrect transformations

; Correct low eight bits do not satisfy the zeroext return contract.
define zeroext i8 @test() {
  ret i8 1
}
