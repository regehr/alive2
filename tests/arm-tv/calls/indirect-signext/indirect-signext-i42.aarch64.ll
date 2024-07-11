define i42 @f(ptr %0, i32 %z) {
  %2 = tail call signext i42 %0(i32 %z)
  ret i42 %2
}
