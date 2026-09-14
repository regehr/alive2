; CHECK: 1 incorrect transformations

; Integers wider than 32 bits must be sign-extended to 64 bits.
define signext i33 @test() {
  ret i33 -1
}
