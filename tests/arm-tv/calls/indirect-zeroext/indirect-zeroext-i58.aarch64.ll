define i58 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i58 %0(i32 %z)
  ret i58 %2
}
