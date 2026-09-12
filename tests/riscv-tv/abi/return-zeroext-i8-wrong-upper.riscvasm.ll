; CHECK: 1 incorrect transformations

; Correct low 32 bits do not excuse nonzero high register bits.
define zeroext i8 @test() {
  ret i8 -1
}
