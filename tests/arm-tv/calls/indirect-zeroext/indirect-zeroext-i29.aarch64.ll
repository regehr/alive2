define i29 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i29 %0(i32 %z)
  ret i29 %2
}
