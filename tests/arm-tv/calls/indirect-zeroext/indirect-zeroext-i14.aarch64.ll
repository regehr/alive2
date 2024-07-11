define i14 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i14 %0(i32 %z)
  ret i14 %2
}
