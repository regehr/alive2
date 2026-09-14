; CHECK: 1 incorrect transformations

; A declaration's zeroext result attribute leaves the upper word unspecified.
declare zeroext i8 @produce()
define i64 @test() {
  %anchor = add i64 0, 0
  %value = call i8 @produce()
  %result = zext i8 %value to i64
  ret i64 %result
}
