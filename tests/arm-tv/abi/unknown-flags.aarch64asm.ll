; CHECK: 1 incorrect transformations

; The initial one-bit Z flag is unspecified.
define i64 @test() memory(none) {
  ret i64 0
}
