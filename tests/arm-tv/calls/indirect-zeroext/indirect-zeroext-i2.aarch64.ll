define i2 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i2 %0(i32 %z)
  ret i2 %2
}
