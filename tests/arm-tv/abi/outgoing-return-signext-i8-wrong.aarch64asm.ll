; CHECK: 1 incorrect transformations

; The return's sign extension must be checked before truncation.
define signext i8 @test() {
  ret i8 -1
}
