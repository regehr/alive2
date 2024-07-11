define i15 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i15 %0(i32 %z)
  ret i15 %2
}
