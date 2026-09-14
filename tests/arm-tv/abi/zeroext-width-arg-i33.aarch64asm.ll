; CHECK: 1 correct transformations

; Above 32 bits, zeroext requires extension all the way to 64 bits.
define i64 @test(i33 zeroext %x) {
  %result = zext i33 %x to i64
  ret i64 %result
}
