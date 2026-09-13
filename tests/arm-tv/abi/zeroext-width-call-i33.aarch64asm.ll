; CHECK: 1 correct transformations

; A zeroext i33 call result already has a defined 64-bit representation.
declare zeroext i33 @produce()
define i64 @test() {
  %anchor = add i64 0, 0
  %value = call zeroext i33 @produce()
  %result = zext i33 %value to i64
  ret i64 %result
}
