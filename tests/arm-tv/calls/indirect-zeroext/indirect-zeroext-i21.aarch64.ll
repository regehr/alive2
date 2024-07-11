define i21 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i21 %0(i32 %z)
  ret i21 %2
}
