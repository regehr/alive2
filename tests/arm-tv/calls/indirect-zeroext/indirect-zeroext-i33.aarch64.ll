define i33 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i33 %0(i32 %z)
  ret i33 %2
}
