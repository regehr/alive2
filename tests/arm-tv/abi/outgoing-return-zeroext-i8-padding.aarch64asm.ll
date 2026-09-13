; CHECK: 1 correct transformations

; A narrow zeroext return leaves bits 63:32 unspecified.
define zeroext i8 @test() {
  ret i8 1
}
