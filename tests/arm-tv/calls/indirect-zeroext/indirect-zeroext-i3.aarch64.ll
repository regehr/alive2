define i3 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i3 %0(i32 %z)
  ret i3 %2
}
