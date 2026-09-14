; CHECK: 1 incorrect transformations

; signext i1 requires ones through bit 31 for true.
define signext i1 @test() {
  ret i1 true
}
