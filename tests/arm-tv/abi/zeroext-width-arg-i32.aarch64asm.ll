; CHECK: 1 correct transformations

; A 32-bit move clears the unspecified upper half; the low word is extended.
define i64 @test(i32 zeroext %x) {
  %result = zext i32 %x to i64
  ret i64 %result
}
