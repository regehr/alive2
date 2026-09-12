; CHECK: 1 correct transformations

; Without an extension attribute, excess register bits remain unspecified.
define i8 @test() {
  ret i8 -1
}
