define i61 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i61 %0(i32 %z)
  ret i61 %2
}
