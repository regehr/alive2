define i28 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i28 %0(i32 %z)
  ret i28 %2
}
