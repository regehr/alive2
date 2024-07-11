define i36 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i36 %0(i32 %z)
  ret i36 %2
}
