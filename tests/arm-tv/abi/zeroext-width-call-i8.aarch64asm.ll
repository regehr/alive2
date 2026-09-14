; CHECK: 1 correct transformations

; Normalizing the low word of a zeroext i8 call result is sufficient.
declare zeroext i8 @produce()
define i64 @test() {
  %anchor = add i64 0, 0
  %value = call zeroext i8 @produce()
  %result = zext i8 %value to i64
  ret i64 %result
}
