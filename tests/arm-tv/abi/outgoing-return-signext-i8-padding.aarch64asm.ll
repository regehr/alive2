; CHECK: 1 correct transformations

; A narrow signext return leaves bits 63:32 unspecified.
define signext i8 @test() {
  ret i8 -1
}
