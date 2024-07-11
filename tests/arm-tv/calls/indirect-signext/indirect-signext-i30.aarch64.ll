define i30 @f(ptr %0, i32 %z) {
  %2 = tail call signext i30 %0(i32 %z)
  ret i30 %2
}
