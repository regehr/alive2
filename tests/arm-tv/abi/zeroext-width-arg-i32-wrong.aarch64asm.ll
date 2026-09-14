; CHECK: 1 incorrect transformations

; The upper 32 register bits are not part of a narrow zeroext contract.
define i64 @test(i32 zeroext %x) {
  %result = zext i32 %x to i64
  ret i64 %result
}
