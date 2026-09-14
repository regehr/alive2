; CHECK: 1 correct transformations

; A poison return may use a representation that is not sign-extended.
define signext i8 @test() {
  ret i8 poison
}
