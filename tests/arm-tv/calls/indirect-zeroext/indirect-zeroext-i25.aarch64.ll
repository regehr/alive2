define i25 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i25 %0(i32 %z)
  ret i25 %2
}
