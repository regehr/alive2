define i16 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i16 %0(i32 %z)
  ret i16 %2
}
