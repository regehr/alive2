define i24 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i24 %0(i32 %z)
  ret i24 %2
}
