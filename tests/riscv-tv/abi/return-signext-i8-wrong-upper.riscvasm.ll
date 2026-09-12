; CHECK: 1 incorrect transformations

; The low word is sign-extended, but the high word must also be all ones.
define signext i8 @test() {
  ret i8 -1
}
