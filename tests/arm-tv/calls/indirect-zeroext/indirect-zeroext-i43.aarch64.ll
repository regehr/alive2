define i43 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i43 %0(i32 %z)
  ret i43 %2
}
