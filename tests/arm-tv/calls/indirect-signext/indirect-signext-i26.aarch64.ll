define i26 @f(ptr %0, i32 %z) {
  %2 = tail call signext i26 %0(i32 %z)
  ret i26 %2
}
