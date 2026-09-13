; CHECK: 1 correct transformations

; A stack-using leaf keeps SP aligned and preserves its return value.
define i64 @test(i64 noundef %x) {
  ret i64 %x
}
