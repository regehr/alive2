; CHECK: 1 correct transformations

; An explicit signext overrides the unsigned Boolean representation.
define signext i1 @test() {
  ret i1 true
}
