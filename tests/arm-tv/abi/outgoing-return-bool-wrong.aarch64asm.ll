; CHECK: 1 incorrect transformations

; A Boolean return must have a low byte of 0 or 1.
define i1 @test() {
  ret i1 true
}
