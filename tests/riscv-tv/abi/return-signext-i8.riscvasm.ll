; CHECK: 1 correct transformations

; A valid negative narrow return is sign-extended through XLEN.
define signext i8 @test() {
  ret i8 -1
}
