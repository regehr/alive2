; CHECK: 1 incorrect transformations

; Even a zeroext i32 call result leaves the upper word unspecified.
declare zeroext i32 @produce()
define i64 @test() {
  %anchor = add i64 0, 0
  %value = call zeroext i32 @produce()
  %result = zext i32 %value to i64
  ret i64 %result
}
