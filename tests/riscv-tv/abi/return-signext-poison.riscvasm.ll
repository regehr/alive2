; CHECK: 1 correct transformations

; Poison permits any return representation, even one without sign extension.
define signext i8 @test() {
  ret i8 poison
}
