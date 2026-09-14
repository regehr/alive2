; CHECK: 1 correct transformations

; A zeroext i8 tail return need not define bits 63:32.
declare zeroext i8 @produce()
define zeroext i8 @test() {
  %anchor = add i64 0, 0
  %value = tail call zeroext i8 @produce()
  ret i8 %value
}
