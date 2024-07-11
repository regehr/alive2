define i12 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i12 %0(i32 %z)
  ret i12 %2
}
