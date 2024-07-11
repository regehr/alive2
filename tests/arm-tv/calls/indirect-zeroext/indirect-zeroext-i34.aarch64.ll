define i34 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i34 %0(i32 %z)
  ret i34 %2
}
