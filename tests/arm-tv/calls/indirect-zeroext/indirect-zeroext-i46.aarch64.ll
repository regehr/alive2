define i46 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i46 %0(i32 %z)
  ret i46 %2
}
