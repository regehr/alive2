define i37 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i37 %0(i32 %z)
  ret i37 %2
}
