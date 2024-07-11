define i53 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i53 %0(i32 %z)
  ret i53 %2
}
