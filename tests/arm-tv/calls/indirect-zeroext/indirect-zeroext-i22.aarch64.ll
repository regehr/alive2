define i22 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i22 %0(i32 %z)
  ret i22 %2
}
