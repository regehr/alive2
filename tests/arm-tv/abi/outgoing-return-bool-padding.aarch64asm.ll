; CHECK: 1 correct transformations

; Bits above the low Boolean byte remain unspecified.
define i1 @test() {
  ret i1 true
}
