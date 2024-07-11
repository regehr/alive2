define i8 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i8 %0(i32 %z)
  ret i8 %2
}
