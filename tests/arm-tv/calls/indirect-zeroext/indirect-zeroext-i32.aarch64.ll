define i32 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i32 %0(i32 %z)
  ret i32 %2
}