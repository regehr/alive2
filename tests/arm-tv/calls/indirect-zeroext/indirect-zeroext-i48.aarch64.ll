define i48 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i48 %0(i32 %z)
  ret i48 %2
}
