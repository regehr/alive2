define i50 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i50 %0(i32 %z)
  ret i50 %2
}
