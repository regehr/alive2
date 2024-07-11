define i6 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i6 %0(i32 %z)
  ret i6 %2
}
