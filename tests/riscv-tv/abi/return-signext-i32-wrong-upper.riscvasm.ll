; CHECK: 1 incorrect transformations

; signext i32 requires extension through all 64 register bits.
define signext i32 @test() {
  ret i32 -1
}
