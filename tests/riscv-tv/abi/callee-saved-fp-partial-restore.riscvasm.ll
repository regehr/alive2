; CHECK: 1 incorrect transformations

; Saving only the low word does not preserve a double-precision saved register.
define void @test() {
  ret void
}
