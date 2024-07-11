define i57 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i57 %0(i32 %z)
  ret i57 %2
}
