define i20 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i20 %0(i32 %z)
  ret i20 %2
}
